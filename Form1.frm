VERSION 5.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   BackColor       =   &H8000000D&
   Caption         =   "CoUnTeR StRiKe CrAsH"
   ClientHeight    =   4785
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6480
   BeginProperty Font 
      Name            =   "Battlefield"
      Size            =   8.25
      Charset         =   2
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H8000000E&
   LinkTopic       =   "Form1"
   Picture         =   "Form1.frx":0000
   ScaleHeight     =   4785
   ScaleWidth      =   6480
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer 
      Interval        =   700
      Left            =   360
      Top             =   3840
   End
   Begin VB.CommandButton cmdSend 
      BackColor       =   &H80000009&
      Caption         =   "CrAsH"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   14.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   2040
      MaskColor       =   &H00000000&
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   3840
      UseMaskColor    =   -1  'True
      Width           =   2415
   End
   Begin VB.TextBox txtAddr 
      Alignment       =   2  'Center
      BackColor       =   &H80000006&
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   330
      Left            =   1920
      TabIndex        =   2
      Text            =   "0.0.0.0"
      Top             =   3360
      Width           =   2655
   End
   Begin VB.TextBox txtMain 
      BackColor       =   &H8000000D&
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000005&
      Height          =   1335
      Left            =   480
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Text            =   "Form1.frx":8765
      Top             =   1560
      Width           =   5535
   End
   Begin VB.Label Label 
      Alignment       =   2  'Center
      BackColor       =   &H8000000D&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "sErVeR aDdReSs:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   375
      Left            =   1920
      TabIndex        =   1
      Top             =   3000
      Width           =   2655
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim runtime As Boolean
Dim step, run As Integer

Private Sub cmdSend_Click()
If runtime = False Then
run = 0
If txtAddr = "0.0.0.0" Then run = 1
If txtAddr Like "0.*.*.*" Then run = 1
If txtAddr Like "*.0.*.*" Then run = 1
If txtAddr Like "*.*.0.*" Then run = 1


If run = 0 And txtAddr Like "*.*.*.*" Then
    runtime = True
    step = 1
Else
    box = MsgBox("EnTeR a SeRvEr AdDrEsS!", vbOKOnly, "ErRoR")
End If
Else
box = MsgBox("HoLd Up i'M cRaShIn HeRe!", vbOKOnly, "ErRoR")
End If
End Sub

Private Sub Form_Load()
runtime = False
End Sub

Private Sub Timer_Timer()
If runtime = True Then
    If step = 1 Then
    txtMain = txtMain & "Connecting to " & txtAddr & vbCrLf
    'Form1.List1.Selected(List1.ListCount - 1) = True
    step = 2
    Else
        If step = 2 Then
    txtMain = txtMain & "Connected to " & txtAddr & vbCrLf
        step = 3
    Else
        If step = 3 Then
    txtMain = txtMain & "USER CrAsH" & vbCrLf
        step = 4
    Else
        If step = 4 Then
    txtMain = txtMain & "331 Password required" & vbCrLf
        step = 5
    Else
        If step = 5 Then
    txtMain = txtMain & "PASS (hidden)" & vbCrLf
        step = 6
    Else
        If step = 6 Then
    txtMain = txtMain & "230 User logged in" & vbCrLf
        step = 7
    Else
        If step = 7 Then
            txtMain = txtMain & "PASV" & vbCrLf
        step = 8
    Else
        If step = 8 Then
    txtMain = txtMain & "227 Entering passive mode (" & Int((99 * Rnd) + 1) & "," & Int((998 * Rnd) + 1) & "," & Int((998 * Rnd) + 1) & "," & Int((998 * Rnd) + 1) & "," & Int((99 * Rnd) + 1) & "," & Int((998 * Rnd) + 1) & "," & Int((998 * Rnd) + 1) & ")" & vbCrLf
    step = 9
    Else
        If step = 9 Then
    txtMain = txtMain & "Opening ASCII data mode for file list" & vbCrLf
        step = 10
    Else
        If step = 10 Then
    txtMain = txtMain & "227 Transfer complete" & vbCrLf
        step = 11
    Else
        If step = 11 Then
    txtMain = txtMain & "STOR CrAsH_sErV.DLL" & vbCrLf
        step = 12
    Else
        If step = 12 Then
    txtMain = txtMain & "!Transfer error: no such file or directory." & vbCrLf
    step = 13
    box = MsgBox(Chr(34) & "CrAsH_sErV.DLL" & Chr(34) & " file not found!            " & vbLf & "Exiting program.", vbCritical, "ErRoR")
    If box = vbOK Then
        End
    End If
    End If
    End If
    End If
    End If
    End If
    End If
    End If
    End If
    End If
    End If
    End If
    End If
End If
End Sub

Private Sub txtMain_Change()
txtMain.SelStart = Len(txtMain)
End Sub
