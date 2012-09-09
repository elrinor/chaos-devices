varying vec3 position;

uniform float maxZ; 
uniform float displayZMax; 
uniform float displayZMin; 

void main() {
	
	if(position.z < displayZMin || position.z > displayZMax)
		discard;
	
	float zRel = position.z / maxZ;
	
	if(zRel < 0.2)
		gl_FragColor = vec4(0.0, zRel / 0.2, 0.0, 1.0);
	else if(zRel < 0.5)
		gl_FragColor = vec4((zRel - 0.2) / 0.3, 1.0, 0.0, 1.0);
	else
		gl_FragColor = vec4(1.0, 1.0 - (zRel - 0.5) / 0.5, 0.0, 1.0);
}