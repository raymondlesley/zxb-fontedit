' -- ---------------------------------------------------------------------- --
' ZX FontEdit - ZX Spectrum application for font and UDG editing
'
' Boriel ZX Basic version - based on zx-fontedit
'
' -- ---------------------------------------------------------------------- --

#include <memcopy.bas>

' -- ---------------------------------------------------------------------- --
' Colour constants

DIM BLACK AS UByte = 0
DIM WHITE AS UByte = 7

' Screen position constants
' DIM PLOT_WIDTH AS Integer = 256
#define PLOT_WIDTH 256
#define PLOT_HEIGHT 192

#define EDIT_PANEL_TOP 1
#define EDIT_PANEL_LEFT 11
#define EDIT_PANEL_WIDTH 10
#define EDIT_PANEL_HEIGHT 10
#define PREVIEW_PANEL_LEFT 26
#define PREVIEW_PANEL_TOP 7
#define PREVIEW_PANEL_WIDTH 3
#define PREVIEW_PANEL_HEIGHT 3
#define PREVIEW_PANEL_X 216  ' (26+1)*8 = x coord of top left
#define PREVIEW_PANEL_Y 64   ' (7+1)*8  = y coord of top left

#define NOTE_PANEL_TOP 20
#define NOTE_PANEL_LEFT 0

#define USER_FONT_AREA $F900
#define FONT_SIZE 768
#define FONT_OFFSET 256   ' 32 * 8 = ASCII 00..31
#define CHAR_OFFSET CODE(" ")

DIM sys_chars AS UInteger AT $5C36  ' 'CHARS' sytem variable - pointer to font
#define SYS_CHARS_DEFAULT  $3C00
DIM sys_rasp AS UByte AT $5C38      ' 'RASP' system variable - length of warning buzz

#define INKEY_SYMB_Q 199
#define INKEY_SYMB_W 201
#define INKEY_SYMB_E 200

#define INKEY_LEFT 8
#define INKEY_DOWN 10
#define INKEY_UP 11
#define INKEY_RIGHT 9

' -- ---------------------------------------------------------------------- --
' on-screen location for each character - starting with ' '
' array [96 printable characters x 2 values] = {row, col}

DIM char_locn(0 TO 95, 0 TO 1) AS UByte => {          _
  {16, 14}, {16, 15}, {16, 16}, {16, 17}, {16, 18}, {16, 19},  _ '  !"#$%
  {16, 20}, {16, 21}, {16, 22}, {16, 23}, {16, 24}, {16, 25},  _ ' &'()*+
  {16, 26}, {16, 27}, {16, 28}, {16, 29},                      _ ' ,-./
  {16, 2},  {16, 3},  {16, 4},  {16, 5},  {16, 6},  {16, 7},   _ ' 012345
  {16, 8},  {16, 9},  {16, 10}, {16, 11},                      _ ' 6789
  {18, 2},  {18, 3},  {18, 4},  {18, 5},  {18, 6},  {18, 7},   _ ' :;<=>?
  {18, 8},  {12, 2},  {12, 3},  {12, 4},  {12, 5},  {12, 6},   _ ' @ABCDE
  {12, 7},  {12, 8},  {12, 9},  {12, 10}, {12, 11}, {12, 12},  _ ' FGHIJK
  {12, 13}, {12, 14}, {12, 15}, {12, 16}, {12, 17}, {12, 18},  _ ' LMNOPQ
  {12, 19}, {12, 20}, {12, 21}, {12, 22}, {12, 23}, {12, 24},  _ ' RSTUVW
  {12, 25}, {12, 26}, {12, 27},                                _ ' XYZ
  {18, 10}, {18, 11}, {18, 12}, {18, 13}, {18, 14}, {18, 15},  _ ' [\]^_£
  {14, 2},  {14, 3},  {14, 4},  {14, 5},  {14, 6},  {14, 7},   _ ' abcdef
  {14, 8},  {14, 9},  {14, 10}, {14, 11}, {14, 12}, {14, 13},  _ ' ghijkl
  {14, 14}, {14, 15}, {14, 16}, {14, 17}, {14, 18}, {14, 19},  _ ' mnopqr
  {14, 20}, {14, 21}, {14, 22}, {14, 23}, {14, 24}, {14, 25},  _ ' stuvwx
  {14, 26}, {14, 27},                                          _ ' yz
  {18, 17}, {18, 18}, {18, 19}, {18, 20}, {18, 21}             _ ' {|}~©
}


' -- ---------------------------------------------------------------------- --

CONST beep_freq AS Float = -16.0
DIM beep_rasp AS Float = sys_rasp
DIM beep_length AS Float = beep_rasp / 256.0

SUB Do_Beep()
	BEEP beep_length, beep_freq
END SUB

' -- ---------------------------------------------------------------------- --
' font routines

SUB Copy_Font(from_addr AS UInteger, to_addr AS UInteger)  ' memory addresses
	' NB: actual font data starts at ' ' character - i.e. 32*8 bytes on
	MemCopy(from_addr + FONT_OFFSET, to_addr + FONT_OFFSET, FONT_SIZE)
END SUB

DIM user_font AS UInteger = SYS_CHARS_DEFAULT

SUB Set_Font(font_base AS UInteger)
	sys_chars = font_base
END SUB

SUB Setup_Font()
	user_font = sys_chars

	' check if current font (CHARS) = default, or already using user font
	IF user_font = SYS_CHARS_DEFAULT THEN
		PRINT AT 3, 1; USER_FONT_AREA
		Copy_Font(SYS_CHARS_DEFAULT, USER_FONT_AREA)

		' set user font
		user_font = USER_FONT_AREA
		Set_Font(user_font)
	END IF
	' else already using user font
END SUB

' -- ---------------------------------------------------------------------- --
' Screen layout Subroutines

SUB Draw_Screen_Border()
	PRINT AT 0, 0; INK WHITE; PAPER BLACK; "/------------------------------\\"
	FOR row = 1 TO 22
		PRINT AT row, 0; INK WHITE; PAPER BLACK; "|"
		PRINT AT row, 31; INK WHITE; PAPER BLACK; "|"
	NEXT row
	PRINT AT 23, 0; INK WHITE; PAPER BLACK; "\\------------------------------/";
END SUB

SUB Draw_Edit_Panel(left AS UByte, top AS UByte)
	PRINT AT top, left; "+--------+"
	DIM bottom AS UByte = top + 9
	DIM right AS UByte = left + 9
	FOR row = top + 1 TO bottom - 1
		PRINT AT row, left; "|"
		PRINT AT row, right; "|"
	NEXT row
	PRINT AT bottom, left; "+--------+"
END SUB

SUB Draw_Preview_Panel(left AS UByte, top AS UByte)
	PRINT AT top, left; "+-+"
	DIM bottom AS UByte = top + 2
	DIM right AS UByte = left + 2
	FOR row = top + 1 TO bottom - 1
		PRINT AT row, left; "|"
		PRINT AT row, right; "|"
	NEXT row
	PRINT AT bottom, left; "+-+"
END SUB

SUB Draw_Edit_to_Preview_Lines()
	' draw edit panel top right to preview panel top left
	DIM line1_startx AS Integer = (EDIT_PANEL_LEFT + EDIT_PANEL_WIDTH) * 8
	DIM line1_starty AS Integer = PLOT_HEIGHT - (EDIT_PANEL_TOP + 1) * 8
	DIM line1_endx AS Integer = (PREVIEW_PANEL_LEFT * 8) - line1_startx
	DIM line1_endy AS Integer = PLOT_HEIGHT - (PREVIEW_PANEL_TOP * 8) - line1_starty

	'PRINT AT 0, 0; line1_startx; " "; line1_starty; " "; line1_endx; " "; line1_endy
	PLOT line1_startx, line1_starty : DRAW line1_endx, line1_endy

	' draw edit window bottom right to preview panel bottom left
	DIM line2_startx AS Integer = (EDIT_PANEL_LEFT + EDIT_PANEL_WIDTH) * 8
	DIM line2_starty AS Integer = PLOT_HEIGHT - (EDIT_PANEL_TOP + EDIT_PANEL_HEIGHT) * 8 + 4
	DIM line2_endx AS Integer = PREVIEW_PANEL_LEFT * 8 - line2_startx
	DIM line2_endy AS Integer = PLOT_HEIGHT - (PREVIEW_PANEL_TOP + PREVIEW_PANEL_HEIGHT) * 8 + 4 - line2_starty

	' PRINT AT 1, 0; line2_startx; " "; line2_starty; " "; line2_endx; " "; line2_endy
	PLOT line2_startx, line2_starty : DRAW line2_endx, line2_endy
END SUB

SUB Draw_Divider(row AS UByte)
	PRINT AT row, 0; INK WHITE; PAPER BLACK; "+------------------------------+"
END SUB

SUB Draw_Character_Row(row AS UByte)
	PRINT AT row, 1; INK WHITE; PAPER BLACK; "                              "
END SUB

SUB Draw_Characters()
	DIM row AS UByte
	DIM col AS UByte

	Draw_Character_Row(12)
	Draw_Character_Row(14)
	Draw_Character_Row(16)
	Draw_Character_Row(18)

	Set_Font(SYS_CHARS_DEFAULT) '  start with ROM font
	FOR char = CODE(" ") TO CODE("\*")
		row = char_locn(char - CHAR_OFFSET,0) : col = char_locn(char - CHAR_OFFSET,1)
		IF row <> 0 THEN
			PRINT AT row, col; INK WHITE; PAPER BLACK; CHR(char)
		END IF
	NEXT char

	Set_Font(user_font) ' switch to user font
	FOR char = CODE(" ") TO CODE("\*")
		row = char_locn(char - CHAR_OFFSET,0) : col = char_locn(char - CHAR_OFFSET,1)
		IF row <> 0 THEN
			PRINT AT row+1, col; CHR(char)
		END IF
	NEXT char
END SUB

SUB Draw_Main_Screen()
    PRINT AT 1, 1; "Fontedit"

    Draw_Screen_Border()
    Draw_Edit_Panel(EDIT_PANEL_LEFT, EDIT_PANEL_TOP)
    Draw_Preview_Panel(PREVIEW_PANEL_LEFT, PREVIEW_PANEL_TOP)
	Draw_Edit_to_Preview_Lines()
	Draw_Divider(EDIT_PANEL_TOP + EDIT_PANEL_HEIGHT)
	Draw_Characters()
	Draw_Divider(20)
END SUB

SUB Edit_Character(character AS UByte)
	' First: show character in preview panel
	PRINT AT PREVIEW_PANEL_TOP+1, PREVIEW_PANEL_LEFT+1; CHR(character)

	' 	Second: load bitmap into edit panel
	DIM chars AS UInteger = user_font : ' (char *)*(sys_chars); // default = 0x3C00
	DIM character_offset AS UInteger = CAST(UInteger, character) << 3 : ' 8* as each char takes 8 bytes
	DIM character_location AS UInteger = (chars + character_offset)
	' PRINT AT EDIT_PANEL_TOP+10, 1; chars; " "; character_offset; " "; character_location

	DIM bitmap(0 TO 7) AS UByte
	DIM grid(0 TO 7, 0 TO 7) AS UByte
	DIM bitmask AS UByte

	FOR row = 0 TO 7
		' get pixel row
		' output pixels
		bitmap(row) = PEEK(character_location)
		' PRINT AT row+2, 1; bitmap(row); "  "

		bitmask = $80 : ' start with leftmost bit
		FOR column = 0 TO 7
			IF bitmap(row) bAND bitmask THEN
				' bit set
				PRINT AT row + EDIT_PANEL_TOP + 1, column + EDIT_PANEL_LEFT + 1; INK WHITE; PAPER BLACK; " "
				grid(column, row) = 1
			ELSE
				' bit reset
				PRINT AT row + EDIT_PANEL_TOP + 1, column + EDIT_PANEL_LEFT + 1; INK BLACK; PAPER WHITE; " "
				grid(column, row) = 0
			END IF

			' next pixel
			bitmask = bitmask >> 1
		NEXT column

		' next row
		character_location = character_location + 1
	NEXT row

	' Third: show edit mode commands in lower panel
	PRINT AT NOTE_PANEL_TOP+1, NOTE_PANEL_LEFT+2; "5678=move;    space=flip    "
	PRINT AT NOTE_PANEL_TOP+2, NOTE_PANEL_LEFT+2; "SYMB+W=write; SYMB+Q=cancel "
	DIM x AS UByte = 0
	DIM y AS UByte = 0
	bitmask = 0x80

	' Fourth: start edit loop
	DIM keypress AS String
	DO  ' edit loop
		' show cursor
		IF grid(x, y) = 1 THEN
			PRINT AT EDIT_PANEL_TOP+y+1, EDIT_PANEL_LEFT+x+1; INK WHITE; PAPER BLACK; "+"
		ELSE
			PRINT AT EDIT_PANEL_TOP+y+1, EDIT_PANEL_LEFT+x+1; INK BLACK; PAPER WHITE; "+"
		END IF

		DO
			keypress = INKEY$
		LOOP WHILE keypress = ""
		PRINT AT 2, 1; "key="; CODE(keypress); "  "

		IF grid(x, y) = 1 THEN
			PRINT AT EDIT_PANEL_TOP+y+1, EDIT_PANEL_LEFT+x+1; INK WHITE; PAPER BLACK; " "
		ELSE
			PRINT AT EDIT_PANEL_TOP+y+1, EDIT_PANEL_LEFT+x+1; INK BLACK; PAPER WHITE; " "
		END IF


		'	rowcol location;
		IF keypress = "5" OR keypress = CHR(INKEY_LEFT) THEN
			' left
			IF x > 0 THEN
				x = x - 1
				bitmask  = bitmask << 1
			END IF
		ELSEIF keypress = "6" OR keypress = CHR(INKEY_DOWN) THEN
			' down
			IF y < 7 THEN
				y = y + 1
			END IF
		ELSEIF keypress = "7" OR keypress = CHR(INKEY_UP) THEN
			' up
			IF y > 0 THEN
				y = y - 1
			END IF
		ELSEIF keypress = "8" OR keypress = CHR(INKEY_RIGHT) THEN
			' right
			IF x < 7 THEN
				x = x + 1
				bitmask = bitmask >> 1
			END IF
		ELSEIF keypress = " " THEN
			' flip bit
			IF grid(x, y) = 1 THEN
				' bitmap(y) bAND bitmask
				' bit set: reset it
				PRINT AT y + EDIT_PANEL_TOP + 1, x + EDIT_PANEL_LEFT + 1; INK BLACK; PAPER WHITE; " "
				bitmap(y) = bitmap(y) bAND bNOT bitmask
				grid(x, y) = 0
				'unplot_xy(PREVIEW_PANEL_X+x, PREVIEW_PANEL_Y+y);
				PLOT OVER 1; PREVIEW_PANEL_X + x, PLOT_HEIGHT - (PREVIEW_PANEL_Y + y) - 1
			ELSE
				' bit reset: set it
				PRINT AT y + EDIT_PANEL_TOP + 1, x + EDIT_PANEL_LEFT + 1; INK WHITE; PAPER BLACK; " "
				bitmap(y) = bitmap(y) bOR bitmask
				grid(x, y) = 1
				'plot_xy(PREVIEW_PANEL_X+x, PREVIEW_PANEL_Y+y);
				PLOT INK BLACK; PREVIEW_PANEL_X + x, PLOT_HEIGHT - (PREVIEW_PANEL_Y + y) - 1
			END IF
		ELSEIF keypress = CHR(INKEY_SYMB_W) THEN
			' save
			character_location = chars + character_offset : ' back to bitmap start

			FOR row = 0 TO 7
				' get pixel row
				' output pixels
				'*character_location++ = bitmap[row];
				POKE character_location, bitmap(row)
				character_location = character_location + 1
			NEXT row
			EXIT DO
		END IF

		DO LOOP WHILE INKEY$ <> ""
	LOOP WHILE keypress <> CHR(INKEY_SYMB_Q)

	' Last: tidy up
	FOR row = 0 TO 7
		FOR column = 0 TO 7
			PRINT AT row + EDIT_PANEL_TOP + 1, column + EDIT_PANEL_LEFT + 1; " "
		NEXT column
	NEXT row
	PRINT AT PREVIEW_PANEL_TOP+1, PREVIEW_PANEL_LEFT+1; " "
	PRINT AT NOTE_PANEL_TOP+1, NOTE_PANEL_LEFT+1; "                            "
	PRINT AT NOTE_PANEL_TOP+2, NOTE_PANEL_LEFT+1; "                            "
END SUB

' -- ---------------------------------------------------------------------- --

#define MENU_TOP   2
#define MENU_LEFT  1

#define MENU_LOAD  1
#define MENU_SAVE  2
#define MENU_RESET 3
#define MENU_QUIT  4

FUNCTION Do_Menu() AS UByte
	PRINT AT MENU_TOP,   MENU_LEFT; "+-------+"
	PRINT AT MENU_TOP+1, MENU_LEFT; "| Load  |\\"
	PRINT AT MENU_TOP+2, MENU_LEFT; "| Save  |\\"
	PRINT AT MENU_TOP+3, MENU_LEFT; "| Reset |\\"
	PRINT AT MENU_TOP+4, MENU_LEFT; "| Quit  |\\"
	PRINT AT MENU_TOP+5, MENU_LEFT; "+-------+\\"
	PRINT AT MENU_TOP+6, MENU_LEFT; " \\\\\\\\\\\\\\\\\\"

	DIM keypress AS String
	DO LOOP WHILE INKEY$ <> "" : ' wait for key up
	DO
		keypress = INKEY$
	LOOP WHILE keypress = ""

	DIM choice AS UByte = 0
	IF keypress = "l" THEN
		choice = MENU_LOAD
	ELSEIF keypress = "s" THEN
		choice = MENU_SAVE
	ELSEIF keypress = "r" THEN
		choice = MENU_RESET
	ELSEIF keypress = "q" THEN
		choice = MENU_QUIT
	ELSE
		Do_Beep()
	END IF

	PRINT AT MENU_TOP,   MENU_LEFT; "         "
	PRINT AT MENU_TOP+1, MENU_LEFT; "          "
	PRINT AT MENU_TOP+2, MENU_LEFT; "          "
	PRINT AT MENU_TOP+3, MENU_LEFT; "          "
	PRINT AT MENU_TOP+4, MENU_LEFT; "          "
	PRINT AT MENU_TOP+5, MENU_LEFT; "          "
	PRINT AT MENU_TOP+6, MENU_LEFT; "          "

	return choice
END FUNCTION

' -- ---------------------------------------------------------------------- --
REM Main routine

BORDER BLACK

PAPER WHITE : INK BLACK : CLS

Setup_Font()
Set_Font(SYS_CHARS_DEFAULT)

Draw_Main_Screen()

' switch to font being edited
Set_Font(user_font)

DIM keypress AS String
DIM last_keypress AS String
DIM char_index AS UByte
DIM character_row AS UByte
DIM character_col AS UByte

DO  ' edit loop
	' print instructions. NB: 0x7F is copyright character
	PRINT AT NOTE_PANEL_TOP+1, NOTE_PANEL_LEFT+2; "Hit char to edit SYMB+E=\*   " : ' \* == (c)
	PRINT AT NOTE_PANEL_TOP+2, NOTE_PANEL_LEFT+2; "                 SYMB+W=menu"

	DO
		keypress = INKEY$
	LOOP WHILE keypress = ""
	'PRINT AT 2, 1; keypress; "="; CODE(keypress); "  "
	IF keypress >= " " AND keypress <= "\*" THEN
		char_index = CODE(keypress) - CHAR_OFFSET
		character_row = char_locn(char_index, 0) + 1
		character_col = char_locn(char_index, 1)
		PRINT AT character_row, character_col; FLASH 1; keypress
		Edit_Character(CODE(keypress))
		last_keypress = keypress
	ELSEIF keypress = CHR(INKEY_SYMB_E) THEN
		' special case: (c) symbol
		char_index = CODE("\*") - CHAR_OFFSET
		character_row = char_locn(char_index, 0) + 1
		character_col = char_locn(char_index, 1)
		PRINT AT character_row, character_col; FLASH 1; "\*"
		Edit_Character(CODE("\*"))
		last_keypress = "\*"
	ELSEIF keypress = CHR(INKEY_SYMB_W) THEN
		' menu
		DIM menu_option AS UByte = Do_Menu()
		IF menu_option = MENU_LOAD THEN
			' load
			'do_load();
		ELSEIF menu_option = MENU_SAVE THEN
			' save
			'char savename[11];  // 10 characters + null terminator
			'ubyte max_length = 10;
			'print_string_at(22, 2, "Save name:           ");
			'input_string(22, 13, savename, 10);
			'do_save(savename);
		ELSEIF menu_option = MENU_RESET THEN
			' reset
			'copy_font((ubyte *)SYS_CHARS_DEFAULT, (ubyte *)*sys_chars);
		ELSEIF menu_option = MENU_QUIT THEN
			' quit
			EXIT DO
		END IF
		Draw_Main_Screen()
	END IF

	IF last_keypress <> "" THEN
		char_index = CODE(last_keypress) - CHAR_OFFSET
		character_row = char_locn(char_index, 0) + 1
		character_col = char_locn(char_index, 1)
		PRINT AT character_row, character_col; last_keypress
	END IF

	DO LOOP WHILE INKEY$ <> ""
LOOP : ' WHILE keypress <> CHR(INKEY_SYMB_Q)

PRINT AT 0, 0;
