#include <iostream>
#include <iterator>

using namespace std;


int main( int argc, char** argv )
{
	string os;
	cin.unsetf(ios::skipws);
	copy( istream_iterator<char>(cin), istream_iterator<char>(),
			back_inserter(os) );

	cout << "RESULT: '" << os << "'\n";

	return 0;
}
