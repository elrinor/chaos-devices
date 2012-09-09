#ifndef __ARX_PREPROCESSOR_H__
#define __ARX_PREPROCESSOR_H__

#include "config.h"

#ifdef ARX_USE_BOOST
#  include <boost/config.hpp>
#  define ARX_JOIN BOOST_JOIN
#  define ARX_STRINGIZE BOOST_STRINGIZE

#  include <boost/preprocessor/logical/bool.hpp>
#  define ARX_BOOL BOOST_PP_BOOL

#  include <boost/preprocessor/if.hpp>
#  define ARX_IF BOOST_PP_IF
#else // ARX_USE_BOOST

/**
 * @def ARX_JOIN(X, Y)
 * Joins the two arguments together, even when one of the arguments is itself a macro
 */
#  define ARX_JOIN(X, Y) ARX_JOIN_I(X, Y)
#  define ARX_JOIN_I(X, Y) ARX_JOIN_II(X, Y)
#  define ARX_JOIN_II(X, Y) X ## Y


/**
 * @def ARX_STRINGIZE(X)
 * Converts the parameter X to a string after macro replacement on X has been performed.
 */
#define ARX_STRINGIZE(X) ARX_STRINGIZE_I(X)
#define ARX_STRINGIZE_I(X) #X


/**
 * @def ARX_BOOL(X)
 * Converts the given INT (i.e. 0 to 256) to BOOL (i.e 0 or 1)
 * 
 * Won't work with mwerks compiler.
 */
#  define ARX_BOOL(x) ARX_BOOL_I(x)
#  define ARX_BOOL_I(x) ARX_BOOL_ ## x
#  define ARX_BOOL_0 0
#  define ARX_BOOL_1 1
#  define ARX_BOOL_2 1
#  define ARX_BOOL_3 1
#  define ARX_BOOL_4 1
#  define ARX_BOOL_5 1
#  define ARX_BOOL_6 1
#  define ARX_BOOL_7 1
#  define ARX_BOOL_8 1
#  define ARX_BOOL_9 1
#  define ARX_BOOL_10 1
#  define ARX_BOOL_11 1
#  define ARX_BOOL_12 1
#  define ARX_BOOL_13 1
#  define ARX_BOOL_14 1
#  define ARX_BOOL_15 1
#  define ARX_BOOL_16 1
#  define ARX_BOOL_17 1
#  define ARX_BOOL_18 1
#  define ARX_BOOL_19 1
#  define ARX_BOOL_20 1
#  define ARX_BOOL_21 1
#  define ARX_BOOL_22 1
#  define ARX_BOOL_23 1
#  define ARX_BOOL_24 1
#  define ARX_BOOL_25 1
#  define ARX_BOOL_26 1
#  define ARX_BOOL_27 1
#  define ARX_BOOL_28 1
#  define ARX_BOOL_29 1
#  define ARX_BOOL_30 1
#  define ARX_BOOL_31 1
#  define ARX_BOOL_32 1
#  define ARX_BOOL_33 1
#  define ARX_BOOL_34 1
#  define ARX_BOOL_35 1
#  define ARX_BOOL_36 1
#  define ARX_BOOL_37 1
#  define ARX_BOOL_38 1
#  define ARX_BOOL_39 1
#  define ARX_BOOL_40 1
#  define ARX_BOOL_41 1
#  define ARX_BOOL_42 1
#  define ARX_BOOL_43 1
#  define ARX_BOOL_44 1
#  define ARX_BOOL_45 1
#  define ARX_BOOL_46 1
#  define ARX_BOOL_47 1
#  define ARX_BOOL_48 1
#  define ARX_BOOL_49 1
#  define ARX_BOOL_50 1
#  define ARX_BOOL_51 1
#  define ARX_BOOL_52 1
#  define ARX_BOOL_53 1
#  define ARX_BOOL_54 1
#  define ARX_BOOL_55 1
#  define ARX_BOOL_56 1
#  define ARX_BOOL_57 1
#  define ARX_BOOL_58 1
#  define ARX_BOOL_59 1
#  define ARX_BOOL_60 1
#  define ARX_BOOL_61 1
#  define ARX_BOOL_62 1
#  define ARX_BOOL_63 1
#  define ARX_BOOL_64 1
#  define ARX_BOOL_65 1
#  define ARX_BOOL_66 1
#  define ARX_BOOL_67 1
#  define ARX_BOOL_68 1
#  define ARX_BOOL_69 1
#  define ARX_BOOL_70 1
#  define ARX_BOOL_71 1
#  define ARX_BOOL_72 1
#  define ARX_BOOL_73 1
#  define ARX_BOOL_74 1
#  define ARX_BOOL_75 1
#  define ARX_BOOL_76 1
#  define ARX_BOOL_77 1
#  define ARX_BOOL_78 1
#  define ARX_BOOL_79 1
#  define ARX_BOOL_80 1
#  define ARX_BOOL_81 1
#  define ARX_BOOL_82 1
#  define ARX_BOOL_83 1
#  define ARX_BOOL_84 1
#  define ARX_BOOL_85 1
#  define ARX_BOOL_86 1
#  define ARX_BOOL_87 1
#  define ARX_BOOL_88 1
#  define ARX_BOOL_89 1
#  define ARX_BOOL_90 1
#  define ARX_BOOL_91 1
#  define ARX_BOOL_92 1
#  define ARX_BOOL_93 1
#  define ARX_BOOL_94 1
#  define ARX_BOOL_95 1
#  define ARX_BOOL_96 1
#  define ARX_BOOL_97 1
#  define ARX_BOOL_98 1
#  define ARX_BOOL_99 1
#  define ARX_BOOL_100 1
#  define ARX_BOOL_101 1
#  define ARX_BOOL_102 1
#  define ARX_BOOL_103 1
#  define ARX_BOOL_104 1
#  define ARX_BOOL_105 1
#  define ARX_BOOL_106 1
#  define ARX_BOOL_107 1
#  define ARX_BOOL_108 1
#  define ARX_BOOL_109 1
#  define ARX_BOOL_110 1
#  define ARX_BOOL_111 1
#  define ARX_BOOL_112 1
#  define ARX_BOOL_113 1
#  define ARX_BOOL_114 1
#  define ARX_BOOL_115 1
#  define ARX_BOOL_116 1
#  define ARX_BOOL_117 1
#  define ARX_BOOL_118 1
#  define ARX_BOOL_119 1
#  define ARX_BOOL_120 1
#  define ARX_BOOL_121 1
#  define ARX_BOOL_122 1
#  define ARX_BOOL_123 1
#  define ARX_BOOL_124 1
#  define ARX_BOOL_125 1
#  define ARX_BOOL_126 1
#  define ARX_BOOL_127 1
#  define ARX_BOOL_128 1
#  define ARX_BOOL_129 1
#  define ARX_BOOL_130 1
#  define ARX_BOOL_131 1
#  define ARX_BOOL_132 1
#  define ARX_BOOL_133 1
#  define ARX_BOOL_134 1
#  define ARX_BOOL_135 1
#  define ARX_BOOL_136 1
#  define ARX_BOOL_137 1
#  define ARX_BOOL_138 1
#  define ARX_BOOL_139 1
#  define ARX_BOOL_140 1
#  define ARX_BOOL_141 1
#  define ARX_BOOL_142 1
#  define ARX_BOOL_143 1
#  define ARX_BOOL_144 1
#  define ARX_BOOL_145 1
#  define ARX_BOOL_146 1
#  define ARX_BOOL_147 1
#  define ARX_BOOL_148 1
#  define ARX_BOOL_149 1
#  define ARX_BOOL_150 1
#  define ARX_BOOL_151 1
#  define ARX_BOOL_152 1
#  define ARX_BOOL_153 1
#  define ARX_BOOL_154 1
#  define ARX_BOOL_155 1
#  define ARX_BOOL_156 1
#  define ARX_BOOL_157 1
#  define ARX_BOOL_158 1
#  define ARX_BOOL_159 1
#  define ARX_BOOL_160 1
#  define ARX_BOOL_161 1
#  define ARX_BOOL_162 1
#  define ARX_BOOL_163 1
#  define ARX_BOOL_164 1
#  define ARX_BOOL_165 1
#  define ARX_BOOL_166 1
#  define ARX_BOOL_167 1
#  define ARX_BOOL_168 1
#  define ARX_BOOL_169 1
#  define ARX_BOOL_170 1
#  define ARX_BOOL_171 1
#  define ARX_BOOL_172 1
#  define ARX_BOOL_173 1
#  define ARX_BOOL_174 1
#  define ARX_BOOL_175 1
#  define ARX_BOOL_176 1
#  define ARX_BOOL_177 1
#  define ARX_BOOL_178 1
#  define ARX_BOOL_179 1
#  define ARX_BOOL_180 1
#  define ARX_BOOL_181 1
#  define ARX_BOOL_182 1
#  define ARX_BOOL_183 1
#  define ARX_BOOL_184 1
#  define ARX_BOOL_185 1
#  define ARX_BOOL_186 1
#  define ARX_BOOL_187 1
#  define ARX_BOOL_188 1
#  define ARX_BOOL_189 1
#  define ARX_BOOL_190 1
#  define ARX_BOOL_191 1
#  define ARX_BOOL_192 1
#  define ARX_BOOL_193 1
#  define ARX_BOOL_194 1
#  define ARX_BOOL_195 1
#  define ARX_BOOL_196 1
#  define ARX_BOOL_197 1
#  define ARX_BOOL_198 1
#  define ARX_BOOL_199 1
#  define ARX_BOOL_200 1
#  define ARX_BOOL_201 1
#  define ARX_BOOL_202 1
#  define ARX_BOOL_203 1
#  define ARX_BOOL_204 1
#  define ARX_BOOL_205 1
#  define ARX_BOOL_206 1
#  define ARX_BOOL_207 1
#  define ARX_BOOL_208 1
#  define ARX_BOOL_209 1
#  define ARX_BOOL_210 1
#  define ARX_BOOL_211 1
#  define ARX_BOOL_212 1
#  define ARX_BOOL_213 1
#  define ARX_BOOL_214 1
#  define ARX_BOOL_215 1
#  define ARX_BOOL_216 1
#  define ARX_BOOL_217 1
#  define ARX_BOOL_218 1
#  define ARX_BOOL_219 1
#  define ARX_BOOL_220 1
#  define ARX_BOOL_221 1
#  define ARX_BOOL_222 1
#  define ARX_BOOL_223 1
#  define ARX_BOOL_224 1
#  define ARX_BOOL_225 1
#  define ARX_BOOL_226 1
#  define ARX_BOOL_227 1
#  define ARX_BOOL_228 1
#  define ARX_BOOL_229 1
#  define ARX_BOOL_230 1
#  define ARX_BOOL_231 1
#  define ARX_BOOL_232 1
#  define ARX_BOOL_233 1
#  define ARX_BOOL_234 1
#  define ARX_BOOL_235 1
#  define ARX_BOOL_236 1
#  define ARX_BOOL_237 1
#  define ARX_BOOL_238 1
#  define ARX_BOOL_239 1
#  define ARX_BOOL_240 1
#  define ARX_BOOL_241 1
#  define ARX_BOOL_242 1
#  define ARX_BOOL_243 1
#  define ARX_BOOL_244 1
#  define ARX_BOOL_245 1
#  define ARX_BOOL_246 1
#  define ARX_BOOL_247 1
#  define ARX_BOOL_248 1
#  define ARX_BOOL_249 1
#  define ARX_BOOL_250 1
#  define ARX_BOOL_251 1
#  define ARX_BOOL_252 1
#  define ARX_BOOL_253 1
#  define ARX_BOOL_254 1
#  define ARX_BOOL_255 1
#  define ARX_BOOL_256 1

/**
 * @def ARX_IF(cond, t, f)
 * If cond is true (i.e. != 0) then evaluates to t, else to f.
 *
 * Won't work with edge compiler.
 */
#  define ARX_IF(cond, t, f) ARX_IF_I(ARX_BOOL(cond), t, f)
#  define ARX_IF_I(bit, t, f) ARX_IF_II(bit, t, f)
#  define ARX_IF_II(bit, t, f) ARX_IF_I_ ## bit(t, f)
#  define ARX_IF_I_0(t, f) f
#  define ARX_IF_I_1(t, f) t

#endif // ARX_USE_BOOST

#define ARX_COMMA ,
#define ARX_LPAREN (
#define ARX_RPAREN )

#define ARX_EMPTY

#define ARX_EMPTY_IT(ARG) ARX_EMPTY
#define ARX_IDENTITY(ARG) ARG
#define ARX_PAREN_IT(ARG) (ARG)
#define ARX_BRACK_IT(ARG) [ARG]
#define ARX_ABRACK_IT(ARG) <ARG>

#endif