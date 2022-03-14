/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "backends/graphics/opengl/shader.h"

#if !USE_FORCED_GLES

#include "common/textconsole.h"
#include "common/util.h"

namespace Common {
DECLARE_SINGLETON(OpenGL::ShaderManager);
}

namespace OpenGL {

namespace {

#pragma mark - Builtin Shader Sources -

const char *const g_defaultVertexShader =
	"attribute vec4 position;\n"
	"attribute vec2 texCoordIn;\n"
	"attribute vec4 blendColorIn;\n"
	"\n"
	"uniform mat4 projection;\n"
	"\n"
	"varying vec2 texCoord;\n"
	"varying vec4 blendColor;\n"
	"\n"
	"void main(void) {\n"
	"\ttexCoord    = texCoordIn;\n"
	"\tblendColor  = blendColorIn;\n"
	"\tgl_Position = projection * position;\n"
	"}\n";

const char *const g_defaultFragmentShader =
	"varying vec2 texCoord;\n"
	"varying vec4 blendColor;\n"
	"\n"
	"uniform sampler2D shaderTexture;\n"
	"\n"
	"void main(void) {\n"
	"\tgl_FragColor = blendColor * texture2D(shaderTexture, texCoord);\n"
	"}\n";

const char *const g_lookUpFragmentShader =
	"varying vec2 texCoord;\n"
	"varying vec4 blendColor;\n"
	"\n"
	"uniform sampler2D shaderTexture;\n"
	"uniform sampler2D palette;\n"
	"\n"
	"const float adjustFactor = 255.0 / 256.0 + 1.0 / (2.0 * 256.0);"
	"\n"
	"void main(void) {\n"
	"\tvec4 index = texture2D(shaderTexture, texCoord);\n"
	"\tgl_FragColor = blendColor * texture2D(palette, vec2(index.a * adjustFactor, 0.0));\n"
	"}\n";

const char *const g_crtEmulationFragmentShader =
	"// PUBLIC DOMAIN CRT STYLED SCAN-LINE SHADER\n"
	"//\n"
	"//   by Timothy Lottes\n"
	"//\n"
	"// This is more along the style of a really good CGA arcade monitor.\n"
	"// With RGB inputs instead of NTSC.\n"
	"// The shadow mask example has the mask rotated 90 degrees for less chromatic aberration.\n"
	"//\n"
	"// Left it unoptimized to show the theory behind the algorithm.\n"
	"//\n"
	"// It is an example what I personally would want as a display option for pixel art games.\n"
	"// Please take and use, change, or whatever.\n"
	"//\n"
	"\n"
	"varying vec2 texCoord;\n"
	"varying vec4 blendColor;\n"
	"\n"
	"uniform sampler2D texture;\n"
	"\n"
	"// Emulated input resolution.\n"
	"vec2 res=blendColor.xy;\n"
	"\n"
	"// Output resolution.\n"
	"vec2 outputRes = blendColor.zw;\n"
	"\n"
	"// Hardness of scanline.\n"
	"//  -8.0 = soft\n"
	"// -16.0 = medium\n"
	"float hardScan=-10.0;\n"
	"\n"
	"// Hardness of pixels in scanline.\n"
	"// -2.0 = soft\n"
	"// -4.0 = hard\n"
	"float hardPix=-4.0;\n"
	"\n"
	"// Hardness of short vertical bloom.\n"
	"//  -1.0 = wide to the point of clipping (bad)\n"
	"//  -1.5 = wide\n"
	"//  -4.0 = not very wide at all\n"
	"float hardBloomScan=-2.0;\n"
	"\n"
	"// Hardness of short horizontal bloom.\n"
	"//  -0.5 = wide to the point of clipping (bad)\n"
	"//  -1.0 = wide\n"
	"//  -2.0 = not very wide at all\n"
	"float hardBloomPix=-1.5;\n"
	"\n"
	"// Amount of small bloom effect.\n"
	"//  1.0/1.0 = only bloom\n"
	"//  1.0/16.0 = what I think is a good amount of small bloom\n"
	"//  0.0     = no bloom\n"
	"float bloomAmount=1.0/16.0;\n"
	"\n"
	"// Display warp.\n"
	"// 0.0 = none\n"
	"// 1.0/8.0 = extreme\n"
	"vec2 warp=vec2(1.0/64.0,1.0/24.0); \n"
	"\n"
	"// Amount of shadow mask.\n"
	"float maskDark=1.0;\n"
	"float maskLight=1.5;\n"
	"\n"
	"//------------------------------------------------------------------------\n"
	"\n"
	"// sRGB to Linear.\n"
	"// Assuing using sRGB typed textures this should not be needed.\n"
	"float ToLinear1(float c){return(c<=0.04045)?c/12.92:pow((c+0.055)/1.055,2.4);}\n"
	"vec3 ToLinear(vec3 c){return vec3(ToLinear1(c.r),ToLinear1(c.g),ToLinear1(c.b));}\n"
	"\n"
	"// Linear to sRGB.\n"
	"// Assuing using sRGB typed textures this should not be needed.\n"
	"float ToSrgb1(float c){return(c<0.0031308?c*12.92:1.055*pow(c,0.41666)-0.055);}\n"
	"vec3 ToSrgb(vec3 c){return vec3(ToSrgb1(c.r),ToSrgb1(c.g),ToSrgb1(c.b));}\n"
	"\n"
	"// Nearest emulated sample given floating point position and texel offset.\n"
	"// Also zero's off screen.\n"
	"vec3 Fetch(vec2 pos,vec2 off){\n"
	"  pos=(floor(pos*res+off)+0.5)/res;\n"
	"  if(max(abs(pos.x-0.5),abs(pos.y-0.5))>0.5)return vec3(0.0,0.0,0.0);\n"
	"  return ToLinear(texture2D(texture,pos.xy,-16.0).rgb);}\n"
	"\n"
	"// Distance in emulated pixels to nearest texel.\n"
	"vec2 Dist(vec2 pos){pos=pos*res;return -((pos-floor(pos))-vec2(0.5));}\n"
	"    \n"
	"// 1D Gaussian.\n"
	"float Gaus(float pos,float scale){return exp2(scale*pos*pos);}\n"
	"\n"
	"// 3-tap Gaussian filter along horz line.\n"
	"vec3 Horz3(vec2 pos,float off){\n"
	"  vec3 b=Fetch(pos,vec2(-1.0,off));\n"
	"  vec3 c=Fetch(pos,vec2( 0.0,off));\n"
	"  vec3 d=Fetch(pos,vec2( 1.0,off));\n"
	"  float dst=Dist(pos).x;\n"
	"  // Convert distance to weight.\n"
	"  float scale=hardPix;\n"
	"  float wb=Gaus(dst-1.0,scale);\n"
	"  float wc=Gaus(dst+0.0,scale);\n"
	"  float wd=Gaus(dst+1.0,scale);\n"
	"  // Return filtered sample.\n"
	"  return (b*wb+c*wc+d*wd)/(wb+wc+wd);}\n"
	"\n"
	"// 5-tap Gaussian filter along horz line.\n"
	"vec3 Horz5(vec2 pos,float off){\n"
	"  vec3 a=Fetch(pos,vec2(-2.0,off));\n"
	"  vec3 b=Fetch(pos,vec2(-1.0,off));\n"
	"  vec3 c=Fetch(pos,vec2( 0.0,off));\n"
	"  vec3 d=Fetch(pos,vec2( 1.0,off));\n"
	"  vec3 e=Fetch(pos,vec2( 2.0,off));\n"
	"  float dst=Dist(pos).x;\n"
	"  // Convert distance to weight.\n"
	"  float scale=hardPix;\n"
	"  float wa=Gaus(dst-2.0,scale);\n"
	"  float wb=Gaus(dst-1.0,scale);\n"
	"  float wc=Gaus(dst+0.0,scale);\n"
	"  float wd=Gaus(dst+1.0,scale);\n"
	"  float we=Gaus(dst+2.0,scale);\n"
	"  // Return filtered sample.\n"
	"  return (a*wa+b*wb+c*wc+d*wd+e*we)/(wa+wb+wc+wd+we);}\n"
	"\n"
	"// 7-tap Gaussian filter along horz line.\n"
	"vec3 Horz7(vec2 pos,float off){\n"
	"  vec3 a=Fetch(pos,vec2(-3.0,off));\n"
	"  vec3 b=Fetch(pos,vec2(-2.0,off));\n"
	"  vec3 c=Fetch(pos,vec2(-1.0,off));\n"
	"  vec3 d=Fetch(pos,vec2( 0.0,off));\n"
	"  vec3 e=Fetch(pos,vec2( 1.0,off));\n"
	"  vec3 f=Fetch(pos,vec2( 2.0,off));\n"
	"  vec3 g=Fetch(pos,vec2( 3.0,off));\n"
	"  float dst=Dist(pos).x;\n"
	"  // Convert distance to weight.\n"
	"  float scale=hardBloomPix;\n"
	"  float wa=Gaus(dst-3.0,scale);\n"
	"  float wb=Gaus(dst-2.0,scale);\n"
	"  float wc=Gaus(dst-1.0,scale);\n"
	"  float wd=Gaus(dst+0.0,scale);\n"
	"  float we=Gaus(dst+1.0,scale);\n"
	"  float wf=Gaus(dst+2.0,scale);\n"
	"  float wg=Gaus(dst+3.0,scale);\n"
	"  // Return filtered sample.\n"
	"  return (a*wa+b*wb+c*wc+d*wd+e*we+f*wf+g*wg)/(wa+wb+wc+wd+we+wf+wg);}\n"
	"\n"
	"// Return scanline weight.\n"
	"float Scan(vec2 pos,float off){\n"
	"  float dst=Dist(pos).y;\n"
	"  return Gaus(dst+off,hardScan);}\n"
	"\n"
	"// Return scanline weight for bloom.\n"
	"float BloomScan(vec2 pos,float off){\n"
	"  float dst=Dist(pos).y;\n"
	"  return Gaus(dst+off,hardBloomScan);}\n"
	"\n"
	"// Allow nearest three lines to effect pixel.\n"
	"vec3 Tri(vec2 pos){\n"
	"  vec3 a=Horz3(pos,-1.0);\n"
	"  vec3 b=Horz5(pos, 0.0);\n"
	"  vec3 c=Horz3(pos, 1.0);\n"
	"  float wa=Scan(pos,-1.0);\n"
	"  float wb=Scan(pos, 0.0);\n"
	"  float wc=Scan(pos, 1.0);\n"
	"  return a*wa+b*wb+c*wc;}\n"
	"\n"
	"// Small bloom.\n"
	"vec3 Bloom(vec2 pos){\n"
	"  vec3 a=Horz5(pos,-2.0);\n"
	"  vec3 b=Horz7(pos,-1.0);\n"
	"  vec3 c=Horz7(pos, 0.0);\n"
	"  vec3 d=Horz7(pos, 1.0);\n"
	"  vec3 e=Horz5(pos, 2.0);\n"
	"  float wa=BloomScan(pos,-2.0);\n"
	"  float wb=BloomScan(pos,-1.0);\n"
	"  float wc=BloomScan(pos, 0.0);\n"
	"  float wd=BloomScan(pos, 1.0);\n"
	"  float we=BloomScan(pos, 2.0);\n"
	"  return a*wa+b*wb+c*wc+d*wd+e*we;}\n"
	"\n"
	"#if 1\n"
	"// Distortion of scanlines, and end of screen alpha.\n"
	"vec2 Warp(vec2 pos){\n"
	"  pos=pos*2.0-1.0;    \n"
	"  pos*=vec2(1.0+(pos.y*pos.y)*warp.x,1.0+(pos.x*pos.x)*warp.y);\n"
	"  return pos*0.5+0.5;}\n"
	" #endif\n"
	"\n"
	" #if 0\n"
	" // No warping\n"
	" vec2 Warp(vec2 pos)\n"
	" {\n"
	" \treturn pos;\n"
	" }\n"
	" #endif\n"
	"\n"
	"#if 1\n"
	"// Very compressed TV style shadow mask.\n"
	"vec3 Mask(vec2 pos){\n"
	"  float line=maskLight;\n"
	"  float odd=0.0;\n"
	"  if(fract(pos.x/6.0)<0.5)odd=1.0;\n"
	"  if(fract((pos.y+odd)/2.0)<0.5)line=maskDark;  \n"
	"  pos.x=fract(pos.x/3.0);\n"
	"  vec3 mask=vec3(maskDark,maskDark,maskDark);\n"
	"  if(pos.x<0.333)mask.r=maskLight;\n"
	"  else if(pos.x<0.666)mask.g=maskLight;\n"
	"  else mask.b=maskLight;\n"
	"  mask*=line;\n"
	"  return mask;}        \n"
	"#endif\n"
	"\n"
	"#if 0\n"
	"// Aperture-grille.\n"
	"vec3 Mask(vec2 pos){\n"
	"  pos.x=fract(pos.x/3.0);\n"
	"  vec3 mask=vec3(maskDark,maskDark,maskDark);\n"
	"  if(pos.x<0.333)mask.r=maskLight;\n"
	"  else if(pos.x<0.666)mask.g=maskLight;\n"
	"  else mask.b=maskLight;\n"
	"  return mask;}        \n"
	"#endif\n"
	"\n"
	"#if 0\n"
	"// Stretched VGA style shadow mask (same as prior shaders).\n"
	"vec3 Mask(vec2 pos){\n"
	"  pos.x+=pos.y*3.0;\n"
	"  vec3 mask=vec3(maskDark,maskDark,maskDark);\n"
	"  pos.x=fract(pos.x/6.0);\n"
	"  if(pos.x<0.333)mask.r=maskLight;\n"
	"  else if(pos.x<0.666)mask.g=maskLight;\n"
	"  else mask.b=maskLight;\n"
	"  return mask;}    \n"
	"#endif\n"
	"\n"
	"#if 0\n"
	"// VGA style shadow mask.\n"
	"vec3 Mask(vec2 pos){\n"
	"  pos.xy=floor(pos.xy*vec2(1.0,0.5));\n"
	"  pos.x+=pos.y*3.0;\n"
	"  vec3 mask=vec3(maskDark,maskDark,maskDark);\n"
	"  pos.x=fract(pos.x/6.0);\n"
	"  if(pos.x<0.333)mask.r=maskLight;\n"
	"  else if(pos.x<0.666)mask.g=maskLight;\n"
	"  else mask.b=maskLight;\n"
	"  return mask;}    \n"
	"#endif\n"
	"\n"
	"\n"
	"// Draw dividing bars.\n"
	"float Bar(float pos,float bar){pos-=bar;return pos*pos<4.0?0.0:1.0;}\n"
	"\n"
	"// Entry.\n"
	"void main( void )\n"
	"{\n"
	"\tvec2 pixelCoord = texCoord * outputRes;\n"
	"\tvec2 pos=Warp(texCoord);\n"
	"\tgl_FragColor.rgb=Tri(pos)*Mask(pixelCoord);\n"
	"\t#if 0\n"
	"\t// Normalized exposure.\n"
	"\t\tgl_FragColor.rgb=mix(gl_FragColor.rgb,Bloom(pos),bloomAmount);\n"
	"\t#else\n"
	"\t// Additive bloom.\n"
	"\t\tgl_FragColor.rgb+=Bloom(pos)*bloomAmount;\n"
	"\t#endif\n"
	"\tgl_FragColor.a=1.0;\n"
	"\tgl_FragColor.rgb=ToSrgb(gl_FragColor.rgb);\n"
	"}";


// Taken from: https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_03#OpenGL_ES_2_portability
const char *const g_precisionDefines =
	"#ifdef GL_ES\n"
	"\t#ifdef GL_FRAGMENT_PRECISION_HIGH\n"
	"\t\tprecision highp float;\n"
	"\t#else\n"
	"\t\tprecision mediump float;\n"
	"\t#endif\n"
	"#else\n"
	"\t#define highp\n"
	"\t#define mediump\n"
	"\t#define lowp\n"
	"#endif\n";

} // End of anonymous namespace

#pragma mark - Uniform Values -

void ShaderUniformInteger::set(GLint location) const {
	GL_CALL(glUniform1i(location, _value));
}

void ShaderUniformFloat::set(GLint location) const {
	GL_CALL(glUniform1f(location, _value));
}

void ShaderUniformMatrix44::set(GLint location) const {
	GL_CALL(glUniformMatrix4fv(location, 1, GL_FALSE, _matrix));
}

#pragma mark - Shader Implementation -

Shader::Shader(const Common::String &vertex, const Common::String &fragment)
	: _vertex(vertex), _fragment(fragment), _isActive(false), _program(0), _uniforms() {
	recreate();
}

Shader::~Shader() {
	// According to extension specification glDeleteObjectARB silently ignores
	// 0. However, with nVidia drivers this can cause GL_INVALID_VALUE, thus
	// we do not call it with 0 as parameter to avoid warnings.
	if (_program) {
		GL_CALL_SAFE(glDeleteProgram, (_program));
	}
}

void Shader::destroy() {
	// According to extension specification glDeleteObjectARB silently ignores
	// 0. However, with nVidia drivers this can cause GL_INVALID_VALUE, thus
	// we do not call it with 0 as parameter to avoid warnings.
	if (_program) {
		GL_CALL(glDeleteProgram(_program));
		_program = 0;
	}
}

bool Shader::recreate() {
	// Make sure any old programs are destroyed properly.
	destroy();

	GLshader vertexShader = compileShader(_vertex.c_str(), GL_VERTEX_SHADER);
	if (!vertexShader) {
		return false;
	}

	GLshader fragmentShader = compileShader(_fragment.c_str(), GL_FRAGMENT_SHADER);
	if (!fragmentShader) {
		GL_CALL(glDeleteShader(vertexShader));
		return false;
	}

	GL_ASSIGN(_program, glCreateProgram());
	if (!_program) {
		GL_CALL(glDeleteShader(vertexShader));
		GL_CALL(glDeleteShader(fragmentShader));
		return false;
	}

	GL_CALL(glAttachShader(_program, vertexShader));
	GL_CALL(glAttachShader(_program, fragmentShader));

	GL_CALL(glLinkProgram(_program));

	GL_CALL(glDetachShader(_program, fragmentShader));
	GL_CALL(glDeleteShader(fragmentShader));

	GL_CALL(glDetachShader(_program, vertexShader));
	GL_CALL(glDeleteShader(vertexShader));

	GLint result;
	GL_CALL(glGetProgramiv(_program, GL_LINK_STATUS, &result));
	if (result == GL_FALSE) {
		GLint logSize;
		GL_CALL(glGetProgramiv(_program, GL_INFO_LOG_LENGTH, &logSize));

		GLchar *log = new GLchar[logSize];
		GL_CALL(glGetProgramInfoLog(_program, logSize, nullptr, log));
		warning("Could not link shader: \"%s\"", log);
		delete[] log;

		destroy();
		return false;
	}

	// Set program object in case shader is active during recreation.
	if (_isActive) {
		GL_CALL(glUseProgram(_program));
	}

	for (UniformMap::iterator i = _uniforms.begin(), end = _uniforms.end(); i != end; ++i) {
		i->_value.location = getUniformLocation(i->_key.c_str());
		i->_value.altered = true;
		if (_isActive) {
			i->_value.set();
		}
	}

	return true;
}

void Shader::activate() {
	// Activate program.
	GL_CALL(glUseProgram(_program));

	// Reset changed uniform values.
	for (UniformMap::iterator i = _uniforms.begin(), end = _uniforms.end(); i != end; ++i) {
		i->_value.set();
	}

	_isActive = true;
}

void Shader::deactivate() {
	_isActive = false;
}

GLint Shader::getAttributeLocation(const char *name) const {
	GLint result = -1;
	GL_ASSIGN(result, glGetAttribLocation(_program, name));
	return result;
}

GLint Shader::getUniformLocation(const char *name) const {
	GLint result = -1;
	GL_ASSIGN(result, glGetUniformLocation(_program, name));
	return result;
}

bool Shader::setUniform(const Common::String &name, ShaderUniformValue *value) {
	UniformMap::iterator uniformIter = _uniforms.find(name);
	Uniform *uniform;

	if (uniformIter == _uniforms.end()) {
		const GLint location = getUniformLocation(name.c_str());
		if (location == -1) {
			delete value;
			return false;
		}

		uniform = &_uniforms[name];
		uniform->location = location;
	} else {
		uniform = &uniformIter->_value;
	}

	uniform->value = Common::SharedPtr<ShaderUniformValue>(value);
	uniform->altered = true;
	if (_isActive) {
		uniform->set();
	}

	return true;
}

GLshader Shader::compileShader(const char *source, GLenum shaderType) {
	const GLchar *versionSource = g_context.type == kContextGLES2 ? "#version 100\n" : "#version 110\n";
	const GLchar *compatSource = shaderType == GL_VERTEX_SHADER ? "" : g_precisionDefines;

	GLshader handle;
	GL_ASSIGN(handle, glCreateShader(shaderType));
	if (!handle) {
		return 0;
	}

	const char *const shaderSources[] = {
		versionSource,
		compatSource,
		source
	};

	GL_CALL(glShaderSource(handle, ARRAYSIZE(shaderSources), shaderSources, nullptr));
	GL_CALL(glCompileShader(handle));

	GLint result;
	GL_CALL(glGetShaderiv(handle, GL_COMPILE_STATUS, &result));
	if (result == GL_FALSE) {
		GLint logSize;
		GL_CALL(glGetShaderiv(handle, GL_INFO_LOG_LENGTH, &logSize));

		GLchar *log = new GLchar[logSize];
		GL_CALL(glGetShaderInfoLog(handle, logSize, nullptr, log));
		warning("Could not compile shader \"%s%s%s\": \"%s\"", versionSource, compatSource, source, log);
		delete[] log;

		GL_CALL(glDeleteShader(handle));
		return 0;
	}

	return handle;
}

ShaderManager::ShaderManager() : _initializeShaders(true) {
	for (int i = 0; i < ARRAYSIZE(_builtIn); ++i) {
		_builtIn[i] = nullptr;
	}
}

ShaderManager::~ShaderManager() {
	for (int i = 0; i < ARRAYSIZE(_builtIn); ++i) {
		delete _builtIn[i];
	}
}

void ShaderManager::notifyDestroy() {
	for (int i = 0; i < ARRAYSIZE(_builtIn); ++i) {
		_builtIn[i]->destroy();
	}
}

void ShaderManager::notifyCreate() {
	if (_initializeShaders) {
		_initializeShaders = false;

		_builtIn[kDefault] = new Shader(g_defaultVertexShader, g_defaultFragmentShader);
		_builtIn[kCLUT8LookUp] = new Shader(g_defaultVertexShader, g_lookUpFragmentShader);
		_builtIn[kCLUT8LookUp]->setUniform1I("palette", 1);
		_builtIn[kCRTEmulationFilter] = new Shader(g_defaultVertexShader, g_crtEmulationFragmentShader);

		for (uint i = 0; i < kMaxUsages; ++i) {
			_builtIn[i]->setUniform1I("shaderTexture", 0);
		}
	} else {
		for (int i = 0; i < ARRAYSIZE(_builtIn); ++i) {
			_builtIn[i]->recreate();
		}
	}
}

Shader *ShaderManager::query(ShaderUsage shader) const {
	if (shader == kMaxUsages) {
		warning("OpenGL: ShaderManager::query used with kMaxUsages");
		return nullptr;
	}

	return _builtIn[shader];
}

} // End of namespace OpenGL

#endif // !USE_FORCED_GLES
