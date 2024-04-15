/**
Uses PortableGL (https://www.github.com/rswinkle/PortableGL)

Copyright (c) 2011-2023 Robert Winkler
Copyright (c) 1997-2023 Fabrice Bellard (clipping code from TinyGL)
*/

#include "Core.h"
#if defined CC_BUILD_GL && defined CC_BUILD_PORTABLEGL
#include "_GraphicsBase.h"
#include "Errors.h"
#include "Window.h"
/* PortableOpen backend (alternative modern-ish backend) */
#define PORTABLEGL_IMPLEMENTATION
#define ROW_MAJOR
#include <portablegl.h>

#define GL_EXTENSIONS            0x1F03
#define GL_VIEWPORT              0x0BA2
#define GL_DEPTH_BITS            0x0D56

void glBindAttribLocation(GLuint program, GLuint index,	const GLchar *name) { }
void glColorMask(GLboolean red,	GLboolean green, GLboolean blue, GLboolean alpha) { }
void glReadPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void * data) { }
/* === END OPENGL HEADERS === */
//
#include "_GLShared.h"
static GfxResourceID white_square;


/*########################################################################################################################*
*-------------------------------------------------------Index buffers-----------------------------------------------------*
*#########################################################################################################################*/
static GLuint GL_GenAndBind(GLenum target) {
	GLuint id;

	glGenBuffers(1, &id);
    glBindBuffer(target, id);
	return id;
}

GfxResourceID Gfx_CreateIb2(int count, Gfx_FillIBFunc fillFunc, void* obj) {
	cc_uint16 indices[GFX_MAX_INDICES];
	GLuint id      = GL_GenAndBind(GL_ELEMENT_ARRAY_BUFFER);
	cc_uint32 size = count * sizeof(cc_uint16);

	fillFunc(indices, count, obj);
	///???glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*9, points, GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	//glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
	return id;
}

void Gfx_BindIb(GfxResourceID ib) { glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, (GLuint)ib); }

void Gfx_DeleteIb(GfxResourceID* ib) {
	GLuint id = (GLuint)(*ib);
	if (!id) return;
	glDeleteBuffers(1, &id);
	*ib = 0;
}


/*########################################################################################################################*
*------------------------------------------------------Vertex buffers-----------------------------------------------------*
*#########################################################################################################################*/
static GfxResourceID Gfx_AllocStaticVb(VertexFormat fmt, int count) {
	return GL_GenAndBind(GL_ARRAY_BUFFER);
}

void Gfx_BindVb(GfxResourceID vb) {
	glBindBuffer(GL_ARRAY_BUFFER, (GLuint)vb);
}

void Gfx_DeleteVb(GfxResourceID* vb) {
	GLuint id = (GLuint)(*vb);
	if (id) glDeleteBuffers(1, &id);
	*vb = 0;
}

void* Gfx_LockVb(GfxResourceID vb, VertexFormat fmt, int count) {
	return FastAllocTempMem(count * strideSizes[fmt]);
}

void Gfx_UnlockVb(GfxResourceID vb) {
	glBufferData(GL_ARRAY_BUFFER, tmpSize, tmpData, GL_STATIC_DRAW);
}


/*########################################################################################################################*
*--------------------------------------------------Dynamic vertex buffers-------------------------------------------------*
*#########################################################################################################################*/
static GfxResourceID Gfx_AllocDynamicVb(VertexFormat fmt, int maxVertices) {
	GLuint id      = GL_GenAndBind(GL_ARRAY_BUFFER);
	cc_uint32 size = maxVertices * strideSizes[fmt];

	glBufferData(GL_ARRAY_BUFFER, size, NULL, GL_DYNAMIC_DRAW);
	return id;
}

void Gfx_BindDynamicVb(GfxResourceID vb) {
	glBindBuffer(GL_ARRAY_BUFFER, (GLuint)vb);
}

void Gfx_DeleteDynamicVb(GfxResourceID* vb) {
	GLuint id = (GLuint)(*vb);
	if (id) glDeleteBuffers(1, &id);
	*vb = 0;
}

void* Gfx_LockDynamicVb(GfxResourceID vb, VertexFormat fmt, int count) {
	return FastAllocTempMem(count * strideSizes[fmt]);
}

void Gfx_UnlockDynamicVb(GfxResourceID vb) {
	glBindBuffer(GL_ARRAY_BUFFER, (GLuint)vb);
	glBufferSubData(GL_ARRAY_BUFFER, 0, tmpSize, tmpData);
}

void Gfx_SetDynamicVbData(GfxResourceID vb, void* vertices, int vCount) {
	cc_uint32 size = vCount * gfx_stride;
	glBindBuffer(GL_ARRAY_BUFFER, (GLuint)vb);
	glBufferSubData(GL_ARRAY_BUFFER, 0, size, vertices);
}


/*########################################################################################################################*
*------------------------------------------------------OpenGL modern------------------------------------------------------*
*#########################################################################################################################*/
#define FTR_TEXTURE_UV (1 << 0)
#define FTR_ALPHA_TEST (1 << 1)
#define FTR_TEX_OFFSET (1 << 2)
#define FTR_LINEAR_FOG (1 << 3)
#define FTR_DENSIT_FOG (1 << 4)
#define FTR_HASANY_FOG (FTR_LINEAR_FOG | FTR_DENSIT_FOG)
#define FTR_FS_MEDIUMP (1 << 7)

#define UNI_MVP_MATRIX (1 << 0)
#define UNI_TEX_OFFSET (1 << 1)
#define UNI_FOG_COL    (1 << 2)
#define UNI_FOG_END    (1 << 3)
#define UNI_FOG_DENS   (1 << 4)
#define UNI_MASK_ALL   0x1F

/* cached uniforms (cached for multiple programs */
static mat4 _view, _proj;
static cc_bool gfx_alphaTest, gfx_texTransform;
static int gfx_fogMode = -1;

struct CCUniforms {
	mat4 mvp;
	vec2 texOffset;
   // sampler2D texImage;
	vec3 fogCol;
	GLfloat fogEnd;
	GLfloat fogDensity;
};
typedef struct CCUniforms CCUniforms;

/* shader programs (emulate fixed function) */
static struct GLShader {
	int features;     /* what features are enabled for this shader */
	int uniforms;     /* which associated uniforms need to be resent to GPU */
	GLuint program;   /* OpenGL program ID (0 if not yet compiled) */
	int locations[5]; /* location of uniforms (not constant) */
  CCUniforms cc_uniforms;
} shaders[6 * 3] = {
	/* no fog */
	{ 0              },
	{ 0              | FTR_ALPHA_TEST },
	{ FTR_TEXTURE_UV },
	{ FTR_TEXTURE_UV | FTR_ALPHA_TEST },
	{ FTR_TEXTURE_UV | FTR_TEX_OFFSET },
	{ FTR_TEXTURE_UV | FTR_TEX_OFFSET | FTR_ALPHA_TEST },
	/* linear fog */
	{ FTR_LINEAR_FOG | 0              },
	{ FTR_LINEAR_FOG | 0              | FTR_ALPHA_TEST },
	{ FTR_LINEAR_FOG | FTR_TEXTURE_UV },
	{ FTR_LINEAR_FOG | FTR_TEXTURE_UV | FTR_ALPHA_TEST },
	{ FTR_LINEAR_FOG | FTR_TEXTURE_UV | FTR_TEX_OFFSET },
	{ FTR_LINEAR_FOG | FTR_TEXTURE_UV | FTR_TEX_OFFSET | FTR_ALPHA_TEST },
	/* density fog */
	{ FTR_DENSIT_FOG | 0              },
	{ FTR_DENSIT_FOG | 0              | FTR_ALPHA_TEST },
	{ FTR_DENSIT_FOG | FTR_TEXTURE_UV },
	{ FTR_DENSIT_FOG | FTR_TEXTURE_UV | FTR_ALPHA_TEST },
	{ FTR_DENSIT_FOG | FTR_TEXTURE_UV | FTR_TEX_OFFSET },
	{ FTR_DENSIT_FOG | FTR_TEXTURE_UV | FTR_TEX_OFFSET | FTR_ALPHA_TEST },
};
static struct GLShader* gfx_activeShader;


// ----- C Shader -----
static void shader_vs(float* vs_output, void* vertex_attribs, Shader_Builtins* builtins, void* uniforms) {
	int uv = gfx_activeShader->features & FTR_TEXTURE_UV;
	int tm = gfx_activeShader->features & FTR_TEX_OFFSET;
	CCUniforms *cc_uniforms = (CCUniforms*)uniforms;

	// attribute vec3 in_pos;
	// attribute vec4 in_col;
	// attribute vec2 in_uv;
	// varying vec4 out_col;
	// varying vec2 out_uv;
	// uniform mat4 mvp;
	// uniform vec2 texOffset;

    // gl_Position = mvp * vec4(in_pos, 1.0);
	builtins->gl_Position = mult_mat4_vec4(cc_uniforms->mvp, ((vec4*)vertex_attribs)[0]);
    // out_col = in_col;
	((vec4*)vs_output)[0] = ((vec4*)vertex_attribs)[1];
    // out_uv  = in_uv;
	if (uv) ((vec2*)vs_output)[1] = ((vec2*)vertex_attribs)[2];
    // out_uv  = out_uv + texOffset;
	if (tm) ((vec2*)vs_output)[1] = add_vec2s(((vec2*)vs_output)[1], ((vec2*)uniforms)[1]);
}

/* Generates source code for a GLSL fragment shader, based on shader's flags */
static void GenFragmentShader(const struct GLShader* shader, cc_string* dst) {
	int uv = shader->features & FTR_TEXTURE_UV;
	int al = shader->features & FTR_ALPHA_TEST;
	int fl = shader->features & FTR_LINEAR_FOG;
	int fd = shader->features & FTR_DENSIT_FOG;
	int fm = shader->features & FTR_HASANY_FOG;


	String_AppendConst(dst,         "void main() {\n");
	if (uv) String_AppendConst(dst, "  vec4 col = texture2D(texImage, out_uv) * out_col;\n");
	else    String_AppendConst(dst, "  vec4 col = out_col;\n");
	if (al) String_AppendConst(dst, "  if (col.a < 0.5) discard;\n");
	if (fm) String_AppendConst(dst, "  float depth = 1.0 / gl_FragCoord.w;\n");
	if (fl) String_AppendConst(dst, "  float f = clamp((fogEnd - depth) / fogEnd, 0.0, 1.0);\n");
	if (fd) String_AppendConst(dst, "  float f = clamp(exp(fogDensity * depth), 0.0, 1.0);\n");
	if (fm) String_AppendConst(dst, "  col.rgb = mix(fogCol, col.rgb, f);\n");
	String_AppendConst(dst,         "  gl_FragColor = col;\n");
	String_AppendConst(dst,         "}");

	String_AppendConst(dst,         "varying vec4 out_col;\n");
	if (uv) String_AppendConst(dst, "varying vec2 out_uv;\n");
	if (uv) String_AppendConst(dst, "uniform sampler2D texImage;\n");
	if (fm) String_AppendConst(dst, "uniform vec3 fogCol;\n");
	if (fl) String_AppendConst(dst, "uniform float fogEnd;\n");
	if (fd) String_AppendConst(dst, "uniform float fogDensity;\n");

	String_AppendConst(dst,         "void main() {\n");
	if (uv) String_AppendConst(dst, "  vec4 col = texture2D(texImage, out_uv) * out_col;\n");
	else    String_AppendConst(dst, "  vec4 col = out_col;\n");
	if (al) String_AppendConst(dst, "  if (col.a < 0.5) discard;\n");
	if (fm) String_AppendConst(dst, "  float depth = 1.0 / gl_FragCoord.w;\n");
	if (fl) String_AppendConst(dst, "  float f = clamp((fogEnd - depth) / fogEnd, 0.0, 1.0);\n");
	if (fd) String_AppendConst(dst, "  float f = clamp(exp(fogDensity * depth), 0.0, 1.0);\n");
	if (fm) String_AppendConst(dst, "  col.rgb = mix(fogCol, col.rgb, f);\n");
	String_AppendConst(dst,         "  gl_FragColor = col;\n");
	String_AppendConst(dst,         "}");
}


void shader_fs(float* fs_input, Shader_Builtins* builtins, void* uniforms) {
  	int uv = gfx_activeShader->features & FTR_TEXTURE_UV;
  	int al = gfx_activeShader->features & FTR_ALPHA_TEST;
  	int fl = gfx_activeShader->features & FTR_LINEAR_FOG;
  	int fd = gfx_activeShader->features & FTR_DENSIT_FOG;
 	int fm = gfx_activeShader->features & FTR_HASANY_FOG;
   vec4 col;
   float depth, f;
	CCUniforms *cc_uniforms = (CCUniforms*)uniforms;

	// varying vec4 out_col;
	// if (uv) varying vec2 out_uv;
	// if (uv) uniform sampler2D texImage;
	// if (fm) uniform vec3 fogCol;
	// if (fl) uniform float fogEnd;
	// if (fd) uniform float fogDensity;

   //if (uv) {
      // vec4 col = texture2D(texImage, out_uv) * out_col
   //}
   //else {
      // vec4 col = out_col;
      col = ((vec4*)fs_input)[0];
   //}

   // if (al) if (col.a < 0.5) discard;
   if (al && col.w < 0.5)
      return;

   if (fm)
      // float depth = 1.0 / gl_FragCoord.w
      depth = 1.0 / builtins->gl_FragCoord.w;

   if (fl)
      // float f = clamp((fogEnd - depth) / fogEnd, 0.0, 1.0);
      f = clamp((cc_uniforms->fogEnd - depth) / cc_uniforms->fogEnd, 0.0, 1.0);

   if (fd)
      // float f = clamp(exp(fogDensity * depth), 0.0, 1.0);
      f = clamp(exp(cc_uniforms->fogDensity * depth), 0.0, 1.0);

   if (fm) {
      // col.rgb = mix(fogCol, col.rgb, f);
      vec4 fogCol = make_vec4(cc_uniforms->fogCol.x, cc_uniforms->fogCol.y, cc_uniforms->fogCol.z, 1.0);
      col = mix_vec4s(fogCol, col, f);
   }

   // gl_FragColor = col;
   builtins->gl_FragColor = col;
}

/* Tries to compile vertex and fragment shaders, then link into an OpenGL program */
static void CompileProgram(struct GLShader* shader) {
	GLuint program;
	GLenum interpolation[4] = { FLAT, FLAT, FLAT, FLAT };

	program = pglCreateProgram(shader_vs, shader_fs, 4, interpolation, GL_FALSE);
	if (!program) Logger_Abort("Failed to create shader program");
	shader->program = program;

	/* Force in_pos/in_col/in_uv attributes to be bound to 0,1,2 locations */
	/* Although most browsers assign the attributes in this order anyways, */
	/* the specification does not require this. (e.g. Safari doesn't) */
	glBindAttribLocation(program, 0, "in_pos");
	glBindAttribLocation(program, 1, "in_col");
	glBindAttribLocation(program, 2, "in_uv");
}

/* Marks a uniform as changed on all programs */
static void DirtyUniform(int uniform) {
	int i;
	for (i = 0; i < Array_Elems(shaders); i++) {
		shaders[i].uniforms |= uniform;
	}
}

/* Sends changed uniforms to the GPU for current program */
static void ReloadUniforms(void) {
	struct GLShader* s = gfx_activeShader;
  pglSetUniform(&s->cc_uniforms);
}

/* Switches program to one that duplicates current fixed function state */
/* Compiles program and reloads uniforms if needed */
static void SwitchProgram(void) {
	struct GLShader* shader;
	int index = 0;

	if (gfx_fogEnabled) {
		index += 6;                       /* linear fog */
		if (gfx_fogMode >= 1) index += 6; /* exp fog */
	}

	if (gfx_format == VERTEX_FORMAT_TEXTURED) index += 2;
	if (gfx_texTransform) index += 2;
	if (gfx_alphaTest)    index += 1;

	shader = &shaders[index];
	if (shader == gfx_activeShader) { ReloadUniforms(); return; }
	if (!shader->program) CompileProgram(shader);

	gfx_activeShader = shader;

	glUseProgram(shader->program);
	ReloadUniforms();
}


/*########################################################################################################################*
*---------------------------------------------------------Textures--------------------------------------------------------*
*#########################################################################################################################*/
void Gfx_BindTexture(GfxResourceID texId) {
	/* Texture 0 has different behaviour depending on backend */
	/*   Desktop OpenGL  - pure white 1x1 texture */
	/*   WebGL/OpenGL ES - pure black 1x1 texture */
	/* So for consistency, always use a 1x1 pure white texture */
	if (!texId) texId = white_square;
	glBindTexture(GL_TEXTURE_2D, (GLuint)texId);
}


/*########################################################################################################################*
*-----------------------------------------------------State management----------------------------------------------------*
*#########################################################################################################################*/
void Gfx_SetFog(cc_bool enabled) { gfx_fogEnabled = enabled; SwitchProgram(); }
void Gfx_SetFogCol(PackedCol color) {
	vec3 colorv = make_vec3(PackedCol_R(color) / 255.0, PackedCol_G(color) / 255.0, PackedCol_B(color) / 255.0);
	if (equal_vec3s(colorv, gfx_activeShader->cc_uniforms.fogCol)) return;
	gfx_activeShader->cc_uniforms.fogCol = colorv;
		
	DirtyUniform(UNI_FOG_COL);
	ReloadUniforms();
}

void Gfx_SetFogDensity(float value) {
	if (gfx_activeShader->cc_uniforms.fogDensity == value) return;
	gfx_activeShader->cc_uniforms.fogDensity = value;
	DirtyUniform(UNI_FOG_DENS);
	ReloadUniforms();
}

void Gfx_SetFogEnd(float value) {
	if (gfx_activeShader->cc_uniforms.fogEnd == value) return;
	gfx_activeShader->cc_uniforms.fogEnd = value;
	DirtyUniform(UNI_FOG_END);
	ReloadUniforms();
}

void Gfx_SetFogMode(FogFunc func) {
	if (gfx_fogMode == func) return;
	gfx_fogMode = func;
	SwitchProgram();
}

void Gfx_SetAlphaTest(cc_bool enabled) { gfx_alphaTest = enabled; SwitchProgram(); }

void Gfx_DepthOnlyRendering(cc_bool depthOnly) {
	cc_bool enabled = !depthOnly;
	SetColorWrite(enabled & gfx_colorMask[0], enabled & gfx_colorMask[1],
				  enabled & gfx_colorMask[2], enabled & gfx_colorMask[3]);
}


/*########################################################################################################################*
*---------------------------------------------------------Matrices--------------------------------------------------------*
*#########################################################################################################################*/
inline static void makeMat4FromMatrix(mat4 *result, const struct Matrix *matrix) {
	(*result)[0] = matrix->row1.x; (*result)[1] = matrix->row1.y; (*result)[2] = matrix->row1.z; (*result)[3] = matrix->row1.w;
	(*result)[4] = matrix->row2.x; (*result)[5] = matrix->row2.y; (*result)[6] = matrix->row2.z; (*result)[7] = matrix->row2.w;
	(*result)[8] = matrix->row3.x; (*result)[9] = matrix->row3.y; (*result)[10] = matrix->row3.z; (*result)[11] = matrix->row3.w;
	(*result)[12] = matrix->row4.x; (*result)[13] = matrix->row4.y; (*result)[14] = matrix->row4.z; (*result)[15] = matrix->row4.w;
}

void Gfx_LoadMatrix(MatrixType type, const struct Matrix* matrix) {
	if (type == MATRIX_VIEW)       makeMat4FromMatrix(&_view, matrix);
	if (type == MATRIX_PROJECTION) makeMat4FromMatrix(&_proj, matrix);

	mult_mat4_mat4(gfx_activeShader->cc_uniforms.mvp, _view, _proj);
	DirtyUniform(UNI_MVP_MATRIX);
	ReloadUniforms();
}
void Gfx_LoadIdentityMatrix(MatrixType type) {
	Gfx_LoadMatrix(type, &Matrix_Identity);
}

void Gfx_EnableTextureOffset(float x, float y) {
	gfx_activeShader->cc_uniforms.texOffset.x = x;
	gfx_activeShader->cc_uniforms.texOffset.y = y;
	gfx_texTransform = true;
	DirtyUniform(UNI_TEX_OFFSET);
	SwitchProgram();
}

void Gfx_DisableTextureOffset(void) {
	gfx_texTransform = false;
	SwitchProgram();
}


/*########################################################################################################################*
*-------------------------------------------------------State setup-------------------------------------------------------*
*#########################################################################################################################*/
static void GLBackend_Init(void) {
    customMipmapsLevels = true;
    const GLubyte* ver  = glGetString(GL_VERSION);
    int major = ver[0] - '0', minor = ver[2] - '0';

    if (major >= 2) return;

    // OpenGL 1.x.. will likely either not work or perform poorly
    cc_string str; char strBuffer[1024];
    String_InitArray_NT(str, strBuffer);
    String_Format2(&str,"Modern OpenGL build requires at least OpenGL 2.0\n" \
                        "Your system only supports OpenGL %i.%i however\n\n" \
                        "As such ClassiCube will likely perform poorly or not work\n" \
                        "It is recommended you use the Normal OpenGL build instead\n",
                        &major, &minor);
    strBuffer[str.length] = '\0';
    Window_ShowDialog("Compatibility warning", strBuffer);
}

static void Gfx_FreeState(void) {
	int i;
	FreeDefaultResources();
	gfx_activeShader = NULL;

	for (i = 0; i < Array_Elems(shaders); i++) {
		glDeleteProgram(shaders[i].program);
		shaders[i].program = 0;
	}
	Gfx_DeleteTexture(&white_square);
}

static void Gfx_RestoreState(void) {
	InitDefaultResources();
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	gfx_format = -1;

	DirtyUniform(UNI_MASK_ALL);
	GL_ClearColor(gfx_clearColor);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDepthFunc(GL_LEQUAL);

	/* 1x1 dummy white texture */
	struct Bitmap bmp;
	BitmapCol pixels[1] = { BITMAPCOLOR_WHITE };
	Bitmap_Init(bmp, 1, 1, pixels);
	Gfx_RecreateTexture(&white_square, &bmp, 0, false);
}
cc_bool Gfx_WarnIfNecessary(void) { return false; }


/*########################################################################################################################*
*----------------------------------------------------------Drawing--------------------------------------------------------*
*#########################################################################################################################*/
typedef void (*GL_SetupVBFunc)(void);
typedef void (*GL_SetupVBRangeFunc)(int startVertex);
static GL_SetupVBFunc gfx_setupVBFunc;
static GL_SetupVBRangeFunc gfx_setupVBRangeFunc;

static void GL_SetupVbColoured(void) {
	glVertexAttribPointer(0, 3, GL_FLOAT,         false, SIZEOF_VERTEX_COLOURED, (void*)0);
	glVertexAttribPointer(1, 4, GL_UNSIGNED_BYTE, true,  SIZEOF_VERTEX_COLOURED, (void*)12);
}

static void GL_SetupVbTextured(void) {
	glVertexAttribPointer(0, 3, GL_FLOAT,         false, SIZEOF_VERTEX_TEXTURED, (void*)0);
	glVertexAttribPointer(1, 4, GL_UNSIGNED_BYTE, true,  SIZEOF_VERTEX_TEXTURED, (void*)12);
	glVertexAttribPointer(2, 2, GL_FLOAT,         false, SIZEOF_VERTEX_TEXTURED, (void*)16);
}

static void GL_SetupVbColoured_Range(int startVertex) {
	cc_uint32 offset = startVertex * SIZEOF_VERTEX_COLOURED;
	glVertexAttribPointer(0, 3, GL_FLOAT,         false, SIZEOF_VERTEX_COLOURED, (void*)(offset));
	glVertexAttribPointer(1, 4, GL_UNSIGNED_BYTE, true,  SIZEOF_VERTEX_COLOURED, (void*)(offset + 12));
}

static void GL_SetupVbTextured_Range(int startVertex) {
	cc_uint32 offset = startVertex * SIZEOF_VERTEX_TEXTURED;
	glVertexAttribPointer(0, 3, GL_FLOAT,         false, SIZEOF_VERTEX_TEXTURED, (void*)(offset));
	glVertexAttribPointer(1, 4, GL_UNSIGNED_BYTE, true,  SIZEOF_VERTEX_TEXTURED, (void*)(offset + 12));
	glVertexAttribPointer(2, 2, GL_FLOAT,         false, SIZEOF_VERTEX_TEXTURED, (void*)(offset + 16));
}

void Gfx_SetVertexFormat(VertexFormat fmt) {
	if (fmt == gfx_format) return;
	gfx_format = fmt;
	gfx_stride = strideSizes[fmt];

	if (fmt == VERTEX_FORMAT_TEXTURED) {
		glEnableVertexAttribArray(2);
		gfx_setupVBFunc      = GL_SetupVbTextured;
		gfx_setupVBRangeFunc = GL_SetupVbTextured_Range;
	} else {
		glDisableVertexAttribArray(2);
		gfx_setupVBFunc      = GL_SetupVbColoured;
		gfx_setupVBRangeFunc = GL_SetupVbColoured_Range;
	}
	SwitchProgram();
}

void Gfx_DrawVb_Lines(int verticesCount) {
	gfx_setupVBFunc();
	glDrawArrays(GL_LINES, 0, verticesCount);
}

void Gfx_DrawVb_IndexedTris_Range(int verticesCount, int startVertex) {
	gfx_setupVBRangeFunc(startVertex);
	glDrawElements(GL_TRIANGLES, ICOUNT(verticesCount), GL_UNSIGNED_SHORT, 0);
}

void Gfx_DrawVb_IndexedTris(int verticesCount) {
	gfx_setupVBFunc();
//	glDrawElements(GL_TRIANGLES, ICOUNT(verticesCount), GL_UNSIGNED_SHORT, 0);
}

void Gfx_BindVb_Textured(GfxResourceID vb) {
	Gfx_BindVb(vb);
	GL_SetupVbTextured();
}

void Gfx_DrawIndexedTris_T2fC4b(int verticesCount, int startVertex) {
	if (startVertex + verticesCount > GFX_MAX_VERTICES) {
		GL_SetupVbTextured_Range(startVertex);
//		glDrawElements(GL_TRIANGLES, ICOUNT(verticesCount), GL_UNSIGNED_SHORT, 0);
		GL_SetupVbTextured();
	} else {
		/* ICOUNT(startVertex) * 2 = startVertex * 3  */
//		glDrawElements(GL_TRIANGLES, ICOUNT(verticesCount), GL_UNSIGNED_SHORT, startVertex * 3);
	}
}
#endif
