//
// Copyright : (c) T.Mishima 2014
// License : Apache-2.0
//

#include <stdio.h>
#include <string.h>
#include <windows.h>

typedef struct {
	HWND hWnd;
	char WindowName[256];
}cell;

// �E�C���h�E�̕������擾���Ė��O����v������E�B���h�E�n���h����n��
BOOL  CALLBACK  EnumWndProc( HWND hWnd, LPARAM lParam )
{
	char buff[256]="";
	GetWindowText( hWnd,buff, sizeof(buff));
	if(strcmp(buff,((cell*)lParam)->WindowName)==0){
		((cell*)lParam)->hWnd = hWnd;
	}
	return TRUE;
}

HWND getWindowHandle(char* wName)
{
	cell c;
	c.hWnd =NULL;
	strcpy(c.WindowName,wName);

	EnumWindows( EnumWndProc, (LPARAM)&c);

        return c.hWnd;
}
