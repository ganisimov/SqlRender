/**
 * @file stringUtilities.cpp
 *
 * This file is part of SQLRender
 *
 * Copyright 2014 Observational Health Data Sciences and Informatics
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @author Observational Health Data Sciences and Informatics
 * @author Martijn Schuemie
 * @author Marc Suchard
 */

#ifndef __stringUtilities_cpp__   // All files should have if-guards
#define __stringUtilities_cpp__

#include <algorithm>
#include <cctype>
#include <functional>
#include <iterator>

#include "stringUtilities.h"

namespace ohdsi {
	namespace stringUtilities {

		String& ltrim(String& s) {
			s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
			return s;
		}

		String& rtrim(String& s) {
			s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
			return s;
		}

		String& trim(String& s) {
			return ltrim(rtrim(s));
		}

// This function is a bit strange.  First, you modify s and then you return a copy.  
// So, both s and the copy are in lower case.
// I have changed the behavior to first make a copy and then transform, such that s is const
// std::string toLowerCase(std::string &s) {
// 	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
// 	return s;
// }

		String toLowerCase(const String& s) {
			String result;
			result.resize(s.size());
			std::transform(s.begin(), s.end(), result.begin(), ::tolower);
			return result;
		}

		String removeParentheses(const String& s) {
			if (s.length() > 1 && ((s.at(0) == '\'' && s.at(s.length() - 1) == '\'') || (s.at(0) == '"' && s.at(s.length() - 1) == '"')))
				return s.substr(1, s.length() - 2);
			else
				return s;
		}

		StringVector multiFindBetween(const String& source, const String& pre, const String& post) { // source, pre and post are not modified, so better to pass reference instead of copy
			typedef std::size_t size_t;

			StringVector values;
			size_t start = 0;
			size_t end = 0;
			while (start != std::string::npos && end != std::string::npos) {
				start = source.find(pre, end);
				if (start != std::string::npos) {
					end = source.find(post, start + pre.length());
					if (end != std::string::npos)
						values.push_back(source.substr(start + pre.length(), end - (start + pre.length())));
				}
			}
			return values;
		}

		String replaceAll(const String& source, const String& search, const String& replace) {
			String result(source);
			size_t pos = 0;
			while ((pos = result.find(search, pos)) != std::string::npos) {
				result.replace(pos, search.length(), replace);
				pos += replace.length();
			}
			return result;
		}

	} // namespace stringUtilities
} // namespace ohdsi

#endif // __stringUtilities_cpp__