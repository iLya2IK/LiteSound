uniform sampler2D Texture0;
uniform sampler1D LSpectre;
uniform sampler1D RSpectre;

uniform float time;

varying vec2 vTexCoord;

void main(void)
{    
    float dx = vTexCoord.x - 0.5;
    float dy = vTexCoord.y - 0.5;
        
    float ang;
    float len = length(vec2(dx, dy));
    
    ang = acos(dy / len);
    float ang_tx = 1.0 - ang / 3.14159265358979; 
 
    vec2 rt = vec2(-0.01 / len) * vec2(dx, dy) + vTexCoord;
    vec3 tr_lst = texture2D(Texture0, rt * vec2(1.0, -1.0)).rgb;        
    
    float lv = (texture1D(LSpectre, ang_tx ).r) * 2.0 + 0.2;
    float rv = (texture1D(RSpectre, ang_tx ).r) * 2.0 + 0.2;    
    
    float linte = 1.0 - clamp(pow(clamp( len - lv,  0.0, 1.0) * 100.0, 3.0), 0.0, 1.0);
    float rinte = 1.0 - clamp(pow(clamp( len - rv,  0.0, 1.0) * 100.0, 3.0), 0.0, 1.0);    
    
    vec3 lnow = vec3(linte * clamp( dx * 100.0, 0.0, 1.0));
    vec3 rnow = vec3(rinte * clamp(-dx * 100.0, 0.0, 1.0));    
    
    float stime = sin(time * 2.0 + ang) * 0.1;
    float ctime = cos(time + ang) * 0.2;
    
    
    gl_FragColor = vec4(tr_lst * vec3(0.9, 0.47, 0.45) + 
                        lnow * vec3(0.2, stime + lv * 0.4 + 0.2, ctime + 0.5) + 
                        rnow * vec3(0.2, ctime + 0.5, stime + rv * 0.4 + 0.2), 1.0);
} 
