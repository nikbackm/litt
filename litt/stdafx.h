// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include "targetver.h"
#define _CRT_SECURE_NO_WARNINGS
#define NOMINMAX
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <stdio.h>
#include <tchar.h>
#include <stdlib.h>
#include <conio.h>

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <memory>
#include <functional>
#include <regex>

#include "sqlite3.h"

#ifdef _PREFAST_
	#define Assert(x) __analysis_assume(!!(x))
#else
	#define Assert(x) _ASSERT(x)
#endif
