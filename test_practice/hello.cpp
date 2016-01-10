#include <iostream>
#include <vector>
using namespace std;



void pt (const vector<string> &vec){
	std::vector<string>::const_iterator itor = vec.begin();
	std::vector<string>::const_iterator end = vec.end();
	for(; itor != end; ++itor){
		cout << *itor << endl;
	}
}

int main(){

	std::vector<string> array;
	for(int i = 0; i < 10; i++){
		array.push_back("pollo");
	}

	//std::cout << "Hello World!" << array[0] << "\n";
	pt(array);

}

