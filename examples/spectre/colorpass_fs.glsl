uniform sampler2D Texture0;

varying vec2 vTexCoord;

void main(void)
{
    vec4 res = texture2D(Texture0, vTexCoord);
    
    gl_FragColor = vec4(res.xyz, 1.0);
}
