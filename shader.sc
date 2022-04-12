using import glm
using import glsl

in uv : vec2 (location = 0)
out color : vec4 (location = 1)
fn main ()
    color = (vec4 (uv * 0.5 + 0.5) 0 1)

print
    compile-glsl 100 'fragment
        static-typify main
