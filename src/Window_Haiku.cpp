#include "Core.h"
#if defined CC_BUILD_HAIKU
extern "C" {
#include "_WindowBase.h"
#include "Graphics.h"
#include "String.h"
#include "Funcs.h"
#include "Bitmap.h"
#include "Errors.h"
#include "Utils.h"
}

#include <AppKit.h>
#include <InterfaceKit.h>
#include <OpenGLKit.h>

static BApplication* app_handle;
static BWindow* win_handle;
static BView* view_handle;
static BGLView* view_3D;
#include <pthread.h>

// Event management
enum CCEventType {
	CC_NONE,
	CC_MOUSE_SCROLL, CC_MOUSE_DOWN, CC_MOUSE_UP, CC_MOUSE_MOVE,
	CC_KEY_DOWN, CC_KEY_UP, CC_KEY_INPUT,
	CC_WIN_RESIZED, CC_WIN_FOCUS, CC_WIN_REDRAW, CC_WIN_QUIT
};
union CCEventValue { float f32; int i32; void* ptr; };
struct CCEvent {
	int type;
	CCEventValue v1, v2;
};

#define EVENTS_DEFAULT_MAX 20
static void* events_mutex;
static int events_count, events_capacity;
static CCEvent* events_list, events_default[EVENTS_DEFAULT_MAX];

static void Events_Init(void) {
	events_mutex    = Mutex_Create();
	events_capacity = EVENTS_DEFAULT_MAX;
	events_list     = events_default;
}

static void Events_Push(const CCEvent* event) {
	Mutex_Lock(events_mutex);
	{
		if (events_count >= events_capacity) {
			Utils_Resize((void**)&events_list, &events_capacity,
						sizeof(CCEvent), EVENTS_DEFAULT_MAX, 20);
		}
		events_list[events_count++] = *event;
	}
	Mutex_Unlock(events_mutex);
}

static cc_bool Events_Pull(CCEvent* event) {
	cc_bool found = false;
	
	Mutex_Lock(events_mutex);
	{
		if (events_count) {
			*event = events_list[0];
			for (int i = 1; i < events_count; i++) {
				events_list[i - 1] = events_list[i];
			}
			events_count--;
			found = true;
		}
	}
	Mutex_Unlock(events_mutex);
	return found;
}

// BApplication implementation
class CC_BApp : public BApplication
{
public:
	CC_BApp();
	void DispatchMessage(BMessage* msg, BHandler* handler);
};

CC_BApp::CC_BApp() : BApplication("application/x-ClassiCube") {
}

void CC_BApp::DispatchMessage(BMessage* msg, BHandler* handler) {
	CCEvent event = { 0 };
	int what = msg->what;
	
	switch (msg->what)
	{
	case B_QUIT_REQUESTED:
		event.type = CC_WIN_QUIT;
		break;
	default:
		//Platform_Log1("APP DISPATCH: %i", &what);
		break;
	}
	if (event.type) Events_Push(&event);
	BApplication::DispatchMessage(msg, handler);
}

// BWindow implementation
class CC_BWindow : public BWindow
{
	public:
		CC_BWindow(BRect frame);
		void DispatchMessage(BMessage* msg, BHandler* handler);
};

CC_BWindow::CC_BWindow(BRect frame) : BWindow(frame, "", B_TITLED_WINDOW, 0) {
}

static void ProcessKeyInput(BMessage* msg) {
	CCEvent event;
	const char* value;
	cc_codepoint cp;
	
	if (msg->FindString("bytes", &value) != B_OK) return;
	if (!Convert_Utf8ToCodepoint(&cp, (const cc_uint8*)value, String_Length(value))) return;
	
	event.type = CC_KEY_INPUT;
	event.v1.i32 = cp;
	Events_Push(&event);
}

static int last_buttons;
static void UpdateMouseButton(int btn, int pressed) {
	CCEvent event;
	event.type   = pressed ? CC_MOUSE_DOWN : CC_MOUSE_UP;
	event.v1.i32 = btn;
	Events_Push(&event);
}

static void UpdateMouseButtons(int buttons) {
	// BeOS API is really odd in that it only provides you with a bitmask
	//  of 'current mouse buttons pressed'
	int changed  = buttons ^ last_buttons;
	
	// TODO move logic to UpdateMouseButton instead?
	if (changed & B_PRIMARY_MOUSE_BUTTON)   
		UpdateMouseButton(KEY_LMOUSE, buttons & B_PRIMARY_MOUSE_BUTTON);
	if (changed & B_SECONDARY_MOUSE_BUTTON)
		UpdateMouseButton(KEY_RMOUSE, buttons & B_SECONDARY_MOUSE_BUTTON);
	if (changed & B_TERTIARY_MOUSE_BUTTON) 
		UpdateMouseButton(KEY_MMOUSE, buttons & B_TERTIARY_MOUSE_BUTTON);
	last_buttons = buttons;
}

void CC_BWindow::DispatchMessage(BMessage* msg, BHandler* handler) {
	CCEvent event = { 0 };
	BPoint where;
	float delta;
	int value, width, height;
	bool active;
	
	switch (msg->what)
	{
	case B_KEY_DOWN:
	case B_UNMAPPED_KEY_DOWN:
		if (msg->FindInt32("key", &value) == B_OK) {
			event.type   = CC_KEY_DOWN;
			event.v1.i32 = value;
		} break;
	case B_KEY_UP:
	case B_UNMAPPED_KEY_UP:
		if (msg->FindInt32("key", &value) == B_OK) {
			event.type   = CC_KEY_UP;
			event.v1.i32 = value;
		} break;
	case B_MOUSE_DOWN:
	case B_MOUSE_UP:
		if (msg->FindInt32("buttons", &value) == B_OK) {
			UpdateMouseButtons(value);
		} break;
	case B_MOUSE_MOVED:
		if (msg->FindPoint("where", &where) == B_OK) {
			event.type   = CC_MOUSE_MOVE;
			event.v1.i32 = where.x;
			event.v2.i32 = where.y;
		} break;
	case B_MOUSE_WHEEL_CHANGED:
		if (msg->FindFloat("be:wheel_delta_y", &delta) == B_OK) {
			event.type   = CC_MOUSE_SCROLL;
			event.v1.f32 = -delta; // negate to match other platforms
		} break;
		
	case B_WINDOW_ACTIVATED:
		if (msg->FindBool("active", &active) == B_OK) {
			event.type   = CC_WIN_FOCUS;
			event.v1.i32 = active;
		} break;
	case B_WINDOW_MOVED:
		break; // avoid unhandled message spam
	case B_WINDOW_RESIZED:
		if (msg->FindInt32("width",  &width)  == B_OK &&
			msg->FindInt32("height", &height) == B_OK) {
			event.type   = CC_WIN_RESIZED;
			event.v1.i32 = width;
			event.v2.i32 = height;
		} break;
	case B_QUIT_REQUESTED:
		Platform_LogConst("WIN_QUIT");
		break;
	case _UPDATE_:
		event.type = CC_WIN_REDRAW;
		break;
	default:
		Platform_LogConst("UNHANDLED MESSAGE:");
		msg->PrintToStream();
		break;
	}
	
	if (event.type) Events_Push(&event);
	if (msg->what == B_KEY_DOWN) ProcessKeyInput(msg);
	BWindow::DispatchMessage(msg, handler);
}


static void AppThread(void) {
	app_handle = new CC_BApp();
	// runs forever
	app_handle->Run();
	delete app_handle;
}

static void RunApp(void) {
	void* thread = Thread_Create(AppThread);
	Thread_Start2(thread, AppThread);
	Thread_Detach(thread);
	
	// wait for BApplication to be started in other thread
	do {
		Thread_Sleep(10);
	} while (!app_handle || app_handle->IsLaunching());
	
	Platform_LogConst("App initialised");
}

void Window_Init(void) {
	Events_Init();
	RunApp();
	BScreen screen(B_MAIN_SCREEN_ID);
	BRect frame = screen.Frame();
	
	// e.g. frame = (l:0.0, t:0.0, r:1023.0, b:767.0)
	//  so have to add 1 here for actual width/height
	DisplayInfo.Width  = frame.IntegerWidth()  + 1;
	DisplayInfo.Height = frame.IntegerHeight() + 1;
	DisplayInfo.ScaleX = 1;
	DisplayInfo.ScaleY = 1;
}

static void DoCreateWindow(int width, int height) {
	// https://www.haiku-os.org/docs/api/classBRect.html#details
	// right/bottom coordinates are inclusive of the coordinates,
	//  so need to subtract 1 to end up with correct width/height
	int x = Display_CentreX(width), y = Display_CentreY(height);
	BRect frame(x, y, x + width - 1, y + height - 1);
	win_handle = new CC_BWindow(frame);
	
	WindowInfo.Exists = true;
	WindowInfo.Handle = win_handle;
	
	frame = win_handle->Bounds();
	WindowInfo.Width  = frame.IntegerWidth()  + 1;
	WindowInfo.Height = frame.IntegerHeight() + 1;
	Platform_Log2("WINDOW: %i, %i", &WindowInfo.Width, &WindowInfo.Height);
}

void Window_Create2D(int width, int height) {
	DoCreateWindow(width, height);
	view_handle = new BView(win_handle->Bounds(), "CC_LAUNCHER",
						B_FOLLOW_LEFT | B_FOLLOW_TOP, 0);
						// B_FOLLOW_ALL, B_FRAME_EVENTS);
	win_handle->AddChild(view_handle);
}

void Window_Create3D(int width, int height) {
	DoCreateWindow(width, height);
	view_3D = new BGLView(win_handle->Bounds(), "CC_GAME",
						B_FOLLOW_LEFT | B_FOLLOW_TOP, 0, 
						// B_FOLLOW_ALL, B_FRAME_EVENTS,
						BGL_RGB | BGL_ALPHA | BGL_DOUBLE | BGL_DEPTH);
	view_handle = view_3D;
	win_handle->AddChild(view_handle);
}

void Window_SetTitle(const cc_string* title) {
	char raw[NATIVE_STR_LEN];
	Platform_EncodeUtf8(raw, title);
	
	win_handle->Lock(); // TODO even need to lock/unlock?
	win_handle->SetTitle(raw);
	win_handle->Unlock();
}

void Clipboard_GetText(cc_string* value) {
}

void Clipboard_SetText(const cc_string* value) {
}

int Window_GetWindowState(void) {
	return WINDOW_STATE_NORMAL;
}

cc_result Window_EnterFullscreen(void) {
	return ERR_NOT_SUPPORTED;
}
cc_result Window_ExitFullscreen(void) { return ERR_NOT_SUPPORTED; }

int Window_IsObscured(void) { return 0; }

void Window_Show(void) {
	win_handle->Lock(); // TODO even need to lock/unlock ?
	win_handle->Show();
	win_handle->Unlock();
}

void Window_SetSize(int width, int height) {
	// ee reason for -1 in DoCreateWindow
	win_handle->Lock(); // TODO even need to lock/unlock ?
	win_handle->ResizeTo(width - 1, height - 1);
	win_handle->Unlock();
}

void Window_Close(void) {
	BMessage* msg = new BMessage(B_QUIT_REQUESTED);
	app_handle->PostMessage(msg);
}

static const cc_uint8 key_map[] = {
	/* 0x00 */ 0,KEY_ESCAPE,KEY_F1,KEY_F2, KEY_F3,KEY_F4,KEY_F5,KEY_F6, 
	/* 0x08 */ KEY_F7,KEY_F8,KEY_F9,KEY_F10, KEY_F11,KEY_F12,KEY_PRINTSCREEN,KEY_SCROLLLOCK,
	/* 0x10 */ KEY_PAUSE,KEY_TILDE,'1','2', '3','4','5','6',
	/* 0x18 */ '7','8','9','0', KEY_MINUS,KEY_EQUALS,KEY_BACKSPACE,KEY_INSERT,
	/* 0x20 */ KEY_HOME,KEY_PAGEUP,KEY_NUMLOCK,KEY_KP_DIVIDE, KEY_KP_MULTIPLY,KEY_KP_MINUS,KEY_TAB,'Q',
	/* 0x28 */ 'W','E','R','T', 'Y','U','I','O',
	/* 0x30 */ 'P',KEY_LBRACKET,KEY_RBRACKET,KEY_BACKSLASH, KEY_DELETE,KEY_END,KEY_PAGEDOWN,KEY_KP7,
	/* 0x38 */ KEY_KP8,KEY_KP9,KEY_KP_PLUS,KEY_CAPSLOCK, 'A','S','D','F',
	/* 0x40 */ 'G','H','J','K', 'L',KEY_SEMICOLON,KEY_QUOTE,KEY_ENTER,	
	/* 0x48 */ KEY_KP4,KEY_KP5,KEY_KP6,KEY_LSHIFT, 'Z','X','C','V',	
	/* 0x50 */ 'B','N','M',KEY_COMMA, KEY_PERIOD,KEY_SLASH,KEY_RSHIFT,KEY_UP,	
	/* 0x58 */ KEY_KP1,KEY_KP2,KEY_KP3,KEY_KP_ENTER, KEY_LCTRL,KEY_LALT,KEY_SPACE,KEY_RALT,	
	/* 0x60 */ KEY_RCTRL,KEY_LEFT,KEY_DOWN,KEY_RIGHT, KEY_KP0,KEY_KP_DECIMAL,KEY_LWIN,0,	
	/* 0x68 */ KEY_RWIN,0,0,0, 0,0,0,0,
};

static int MapNativeKey(int raw) {
	int key = raw >= 0 && raw < Array_Elems(key_map) ? key_map[raw] : 0;
	if (!key) Platform_Log2("Unknown key: %i (%h)", &raw, &raw);
	return key;
}

void Window_ProcessEvents(void) {
	CCEvent event;
	int key;
	
	while (Events_Pull(&event))
	{
		//Platform_Log1("GO: %i", &event.type);
		switch (event.type)
		{
		case CC_MOUSE_SCROLL:
			Mouse_ScrollWheel(event.v1.f32);
			break;
		case CC_MOUSE_DOWN:
			Input_SetPressed(event.v1.i32);
			break;
		case CC_MOUSE_UP:
			Input_SetReleased(event.v1.i32);
			break; 
		case CC_MOUSE_MOVE:
			//Platform_Log2("POS: %i,%i", &event.v1.i32, &event.v2.i32);
			Pointer_SetPosition(0, event.v1.i32, event.v2.i32);
			break;
		case CC_KEY_DOWN:
			key = MapNativeKey(event.v1.i32);
			if (key) Input_SetPressed(key);
			break; 
		case CC_KEY_UP:
			key = MapNativeKey(event.v1.i32);
			if (key) Input_SetReleased(key);
			break;
		case CC_KEY_INPUT:
			Event_RaiseInt(&InputEvents.Press, event.v1.i32);
			break;
		case CC_WIN_RESIZED:
			WindowInfo.Width  = event.v1.i32;
			WindowInfo.Height = event.v2.i32;
			Platform_Log2("WINIE: %i,%i", &WindowInfo.Width, &WindowInfo.Height);
			Event_RaiseVoid(&WindowEvents.Resized);
			break;
		case CC_WIN_FOCUS:
			WindowInfo.Focused = event.v1.i32;
			Event_RaiseVoid(&WindowEvents.FocusChanged);
			break;
		case CC_WIN_REDRAW:
			Event_RaiseVoid(&WindowEvents.RedrawNeeded);
			break;
		case CC_WIN_QUIT:
			WindowInfo.Exists = false;
			Event_RaiseVoid(&WindowEvents.Closing);
			break;
		}
	}
}

static void Cursor_GetRawPos(int* x, int* y) {
	BPoint where;
	uint32 buttons;
	
	win_handle->Lock();
	view_handle->GetMouse(&where, &buttons, false);
	win_handle->Unlock();
	
	// TODO: Should checkQueue be true
	*x = (int)where.x;
	*y = (int)where.y;
}
void Cursor_SetPosition(int x, int y) {
	// https://discourse.libsdl.org/t/sdl-mouse-bug/597/11
	BRect frame = win_handle->Frame();
	set_mouse_position(frame.left + x, frame.top + y);
}

static void Cursor_DoSetVisible(cc_bool visible) {
	if (visible) {
		app_handle->ShowCursor();
	} else {
		app_handle->HideCursor();
	}
}

static void ShowDialogCore(const char* title, const char* msg) {
	BAlert* alert = new BAlert(title, msg, "OK");
	// doesn't show title by default
	alert->SetLook(B_TITLED_WINDOW_LOOK);
	alert->Go();
}

cc_result Window_OpenFileDialog(const char* const* filters, OpenFileDialogCallback callback) {
	return ERR_NOT_SUPPORTED;
}

static BBitmap* win_framebuffer;
static struct Bitmap win_fb;
void Window_AllocFramebuffer(struct Bitmap* bmp) {
	BRect bounds(0, 0, bmp->width, bmp->height);
	win_framebuffer = new BBitmap(bounds, B_RGB32);
	
	bmp->scan0 = (BitmapCol*)Mem_Alloc(bmp->width * bmp->height, 4, "framebuffer pixels");
	win_fb = *bmp;
}

void Window_DrawFramebuffer(Rect2D r) {
	void* dst_pixels = win_framebuffer->Bits();
	int32 dst_stride = win_framebuffer->BytesPerRow();
	
	Platform_Log4("DRAW: %i,%i -- %i,%i", &r.X, &r.Y, &r.Width, &r.Height);
	int BL = win_framebuffer->BitsLength(), BR = win_framebuffer->BytesPerRow();
	Platform_Log2("   %i / %i", &BR, &BL);
	
	// TODO redo Bitmap so it supports strides
	for (int y = r.Y; y < r.Y + r.Height; y++)
	{
		BitmapCol* src = Bitmap_GetRow(&win_fb, y) + r.X;
		char*  dst     = (char*)dst_pixels + dst_stride * y + r.X * 4;
		Mem_Copy(dst, src, r.Width * 4);
	}
	
	BRect rect(r.X, r.Y, r.X + r.Width, r.Y + r.Height);
	win_handle->Lock();
	view_handle->DrawBitmap(win_framebuffer, rect, rect);
	win_handle->Unlock();
}

void Window_FreeFramebuffer(struct Bitmap* bmp) {
	delete win_framebuffer;
	Mem_Free(bmp->scan0);
	bmp->scan0 = NULL;
}

void Window_OpenKeyboard(struct OpenKeyboardArgs* args) { }
void Window_SetKeyboardText(const cc_string* text) { }
void Window_CloseKeyboard(void) {  }

void Window_EnableRawMouse(void)  { DefaultEnableRawMouse(); }
void Window_UpdateRawMouse(void)  { DefaultUpdateRawMouse(); }
void Window_DisableRawMouse(void) { DefaultDisableRawMouse(); }


/*########################################################################################################################*
*-----------------------------------------------------OpenGL context------------------------------------------------------*
*#########################################################################################################################*/
#if defined CC_BUILD_GL && !defined CC_BUILD_EGL
static cc_bool win_vsync;

void GLContext_Create(void) {
	view_3D->LockGL();
}

void GLContext_Update(void) { }
cc_bool GLContext_TryRestore(void) { return true; }
void GLContext_Free(void) {
	view_3D->UnlockGL();
}

void* GLContext_GetAddress(const char* function) {
	return view_3D->GetGLProcAddress(function);
}

cc_bool GLContext_SwapBuffers(void) {
	view_3D->SwapBuffers(win_vsync);
	return true;
}

void GLContext_SetFpsLimit(cc_bool vsync, float minFrameMs) {
	win_vsync = vsync;
}
void GLContext_GetApiInfo(cc_string* info) { }
#endif
#endif
