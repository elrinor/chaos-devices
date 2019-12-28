#include <fstream>
#include <iostream>
#include <string>
#include <deque>
#include <string_view>
#include <algorithm>
#include <utility>

#include "sha1.h"
#include "MemoryMapped.h"

void printHelp() {
	std::cout << "pwncheck - password pwn checker for haveibeenpwned.com" << std::endl;
	std::cout << std::endl;
	std::cout << "Usage:" << std::endl;
	std::cout << "pwncheck <passwords.txt>" << std::endl;
}

std::string sha1(std::string_view data) {
	SHA1 ctx;
	ctx.update(std::string(data));
	return ctx.final();
}

std::string toLower(std::string_view src) {
	std::string result(src.size(), '\0');

	std::transform(
		src.begin(), 
		src.end(),
		result.begin(), 
		[](unsigned char c) { return std::tolower(c); }
	);

	return result;
}

bool isCrLf(char c) {
	return c == '\r' || c == '\n';
}

std::string extractEntry(std::string_view map, size_t pos) {
	size_t start = pos;

	while (start > 0 && isCrLf(map[start - 1]))
		start--;
	while (start > 0 && !isCrLf(map[start - 1]))
		start--;

	size_t end = start;
	while (end < map.size() && !isCrLf(map[end]))
		end++;
	
	return toLower(map.substr(start, end - start));
}

int findEntry(std::string_view map, std::string_view hash) {
	size_t l = 0;
	size_t r = map.size();

	while (l < r) {
		size_t m = (l + r) / 2;

		std::string entry = extractEntry(map, m);

		if (entry.starts_with(hash))
			return std::stoi(entry.substr(hash.size() + 1));

		if (entry < hash) {
			l = m + 1;
		} else {
			r = m;
		}
	}

	return -1;
}

int main(int argc, char** argv) {
	using namespace std::string_literals;

	if (argc != 2) {
		printHelp();
		return 0;
	}

	try {
		MemoryMapped file(argv[1]);
		std::string_view map(reinterpret_cast<const char*>(file.getData()), file.size());

		while (true) {
			std::cout << "Enter password: ";

			std::string line;
			if (!std::getline(std::cin, line))
				break;

			std::string hash = sha1(line);

			int count = findEntry(map, hash);
			if (count < 0) {
				std::cout << "Safe!" << std::endl;
			} else {
				std::cout << "Pwned! " << hash << ":" << count << std::endl;
			}

			std::cout << std::endl;
		}
	} catch (const std::exception& e) {
		std::cout << e.what();
		return 1;
	}
    
	printHelp();
	return 0;
}

