module Elm.TypeInference.Type.ExternalTypes exposing
    ( mat4
    , texture
    , vec2
    , vec3
    , vec4
    )

import Elm.TypeInference.ModuleIds as ModuleIds
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType)


vec2 : MonoType
vec2 =
    TypeI.external
        "elm-explorations/linear-algebra"
        ModuleIds.mathVector2Id
        "Vec2"


vec3 : MonoType
vec3 =
    TypeI.external
        "elm-explorations/linear-algebra"
        ModuleIds.mathVector3Id
        "Vec3"


vec4 : MonoType
vec4 =
    TypeI.external
        "elm-explorations/linear-algebra"
        ModuleIds.mathVector4Id
        "Vec4"


mat4 : MonoType
mat4 =
    TypeI.external
        "elm-explorations/linear-algebra"
        ModuleIds.mathMatrix4Id
        "Mat4"


texture : MonoType
texture =
    TypeI.external
        "elm-explorations/webgl"
        ModuleIds.webGLTextureId
        "Texture"
