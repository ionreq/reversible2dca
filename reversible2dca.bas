' reversible 2d automata
'

Const NX = 64  ' buffers size
Const NY = 64
Const NB = 100		' n buffers
Const BB = 1
Const BS = 16

Dim Shared As Byte buf(NB,NX,NY)
Dim Shared As Byte snm(9,2)

Declare Sub main
main
End


' display buffer n at screen position px,py
'
Sub display (n As Integer, px As Integer, py As Integer)
	Dim As Integer x, y, b
	Dim As ULong col

	For y = 0 To NY-1
	For x = 0 To NX-1
		b = buf(n,x,y)
		If b=0 Then col = RGB(0,0,0) 
		If b=1 Then col = RGB(255,255,255) 
		Line(px+x*BB,py+y*BB)-(px+(x+1)*BB-1,py+(y+1)*BB-1),col,bf
	Next
	Next
End Sub


' display buffer snm matrix at screen position px,py
'
Sub displaysnm (px As Integer, py As Integer)
	Dim As Integer x, y, b
	Dim As ULong col

	For y = 0 To 1
	For x = 0 To 8
		b = snm(x,y)
		If b=0 Then col = RGB(0,0,0) 
		If b=1 Then col = RGB(255,255,255) 
		Line(px+x*BS,py+y*BS)-(px+(x+1)*BS-2,py+(y+1)*BS-2),col,bf
	Next
	Next
End Sub


' init buffer n randomly
'
Sub initrnd (n As Integer)
	Dim As Integer x, y
	
	For y = 0 To NY-1
	For x = 0 To NX-1
		If Rnd<0.5 Then buf(n,x,y) = 1 : Else buf(n,x,y) = 0
	Next
	Next
End Sub


' init buffer a with bitmap image n
'
Sub initbmp (a As Integer, n As Integer)
	Dim As Integer x,y
	Dim As String s

	Dim img As Any Ptr = ImageCreate (NX, NY)
	s = "pic"+Str(n)+".bmp" 
	BLoad s, img
	Put (0,0), img, PSet
	For y = 0 To NY-1
	For x = 0 To NX-1
		If Point(x,y)=RGB(0,0,0) Then buf(a,x,y)=1
	Next
	Next	
	ImageDestroy img
End Sub


' clear buffer n
'
Sub clearbuf (n As Integer)
	Dim As Integer x, y
	
	For y = 0 To NY-1
	For x = 0 To NX-1
		buf(n,x,y) = 0
	Next
	Next
End Sub


' are buffers a and b identical?
'
Function equal (a As Integer, b As Integer) As Integer
	Dim As Integer x, y, gef
	
	gef=1
	For y = 0 To NY-1
	For x = 0 To NX-1
		If buf(a,x,y)<>buf(b,x,y) Then gef=0
	Next
	Next
	Return gef
End Function


' copy buffer a to b
'
Sub copy (a As Integer, b As Integer)
	Dim As Integer x, y
	
	For y = 0 To NY-1
	For x = 0 To NX-1
		buf(b,x,y )= buf(a,x,y)
	Next
	Next
End Sub


' count neighbors in buffer a at x,y
'
Function cntnb (a As Integer, x As Integer, y As Integer) As Integer
	Dim As Integer dx, dy, mx, my, n

	For dy = -1 To 1
	For dx = -1 To 1
		mx = x+dx : If mx<0 Then mx+=NX : Else If mx>NX-1 Then mx-=NX
		my = y+dy : If my<0 Then my+=NY : Else If my>NY-1 Then my-=NY
		If dx<>0 Or dy<>0 Then
			n += buf(a,mx,my)
		EndIf
	Next
	Next
	Return n
End Function


' do a generation from buffers a and b at x,y
'
Function genxy (a As Integer, b As Integer, x As Integer, y As Integer) As Integer
	Return snm(cntnb (b,x,y), buf(a,x,y))
End Function


' do a generation from buffer a and b to buffer c
'
Sub gen (a As Integer, b As Integer, c As Integer)
	Dim As Integer x, y, n, m
	
	For y = 0 To NY-1
	For x = 0 To NX-1
		buf(c,x,y) = genxy (a, b, x, y)
	Next
	Next
End Sub


' test if buffers a and b contain all possible n,m combinations
'
Function allconfigs (a As Integer, b As Integer) As Integer
	Dim As Integer x, y
	Dim As Integer n, m, gef
	Dim As Integer tsnm(9,2)
	
	For y = 0 To NY-1
	For x = 0 To NX-1
		m = buf(a,x,y)
		n = cntnb(b,x,y)
		tsnm(n,m) = 1
	Next
	Next
	
	gef = 1
	For y = 0 To 1
	For x = 0 To 8
		If tsnm(x,y)=0 Then gef = 0
	Next
	Next
	
	Return gef
End Function


' init snm with bits from number c
'
Sub initsnm (c As Integer)
	Dim As Integer x,y

	For y = 0 To 1
	For x = 0 To 8
	  	snm(x,y) = Abs(0<(c And (1 Shl (x+y*9))))
	Next
	Next
End Sub


' main
'
Sub main()
	Dim As Integer t, sc, ig, x, y
	Dim As String i

	Screen 19,32,2
	ScreenSet 1,0
	Randomize Timer
	Color RGB(255,255,255),RGB(125,125,125)
	Cls

	initbmp (91, 1)	' load and store images
	initbmp (92, 2)

	sc = 0	' snm counter
	ig = 0	' found reversible counter
	Do
		Do
			initsnm (sc)
			sc+=1
			If sc=2^18 Then Exit Do

			'Do					' two random images
			'	initrnd (1)
			'	initrnd (2)
			'Loop While 0=allconfigs(1,2)
			copy (91,11)		' the bmp images
			copy (92,12)

			copy (11, 88)			' store for comparison
			For t = 11+2 To 35		' forward
				gen (t-2, t-1, t)
			Next
			'For t = 1 To 35-2		' in case you don't trust it
			'	clearbuf (t)
			'Next
			For t = 35-2 To 1 Step -1		' backward
				gen (t+2, t+1, t)
			Next

			If equal (11, 88) Then Exit Do	' found one
			'Exit do		' displays all the configurations
			i = Inkey
			If i<>"" Then Exit Do
		Loop
		ig +=1

		Cls
		Locate 37,30:Print sc, ig
		displaysnm (40, 5*(NY+40)+40)
		x = 40
		y = 40
		For t = 1 To 35
			display (t, x, y)
			x += NX+40
			If x>750 Then
				x = 40
				y += NY+40
			EndIf
		Next
		ScreenCopy
	
		If sc=2^18 Then Sleep
		
		If i="q" Or i=Chr(27) Then Exit Do
	Loop
	
End Sub
