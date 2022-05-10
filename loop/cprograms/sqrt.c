#include <stdio.h>


int floorsqrt(int x)
{
	int res = 0;
	for(int i =1 ; i <= x; i++){
		if(i*i == x){
			res = i;
		}
		if(i*i > x && res == 0){
			res = i - 1;
		}
	}
	return res;
}


int main(){

	printf("%d %d %d %d %d\n", floorsqrt(2), floorsqrt(4), floorsqrt(67), floorsqrt(101), floorsqrt(180));
	return 0;
}