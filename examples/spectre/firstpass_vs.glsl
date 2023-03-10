#version 330 core

varying vec2 vTexCoord;

layout(location = 0) in vec2 vertexPos;
layout(location = 1) in vec2 texPos;

void main(void)
{
    vTexCoord = texPos;
    gl_Position = vec4(vertexPos.xy,0.0,1.0);
}
