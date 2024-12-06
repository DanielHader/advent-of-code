*> Compiled using GnuCOBOL "cobc" with flags "-std=cobol85 --free"
IDENTIFICATION DIVISION.
PROGRAM-ID. Advent-Of-Code-Day04.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Input-File
        ASSIGN TO "input.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
        
DATA DIVISION.
FILE SECTION.
FD Input-File.
01 Input-Row                    PIC A(200) VALUE " ".
        
WORKING-STORAGE SECTION.
01 Grid-Data.       
    05 Grid-Row                 OCCURS 200 TIMES.
        10 Grid-Column          OCCURS 200 TIMES.
            15 Grid-Character   PIC A(1).

01 Found-Match                  PIC A(1).
01 End-Of-File                  PIC A(1).
01 Idx                          PIC 9(3).

01 Row-Count                    PIC 9(3) VALUE 0.
01 Col-Count                    PIC 9(3) VALUE 0.
01 Start-Row                    PIC 9(3).
01 Start-Col                    PIC 9(3).
01 Current-Row                  PIC 9(3).
01 Current-Col                  PIC 9(3).

01 Search-Length                PIC 9(1) VALUE 4.
01 Search-String                PIC A(4) VALUE "XMAS".

01 Search-Directions.
    05 Search-Deltas            OCCURS 8 TIMES.
        10 Search-DX            PIC S9(1).
        10 Search-DY            PIC S9(1).

01 Dir                          PIC 9(1).
01 S-Idx1                       PIC S9(1).
01 S-Idx2                       PIC S9(1).

01 Matches-Found                PIC 9(10).

PROCEDURE DIVISION.
    PERFORM INIT-SEARCH-DIRECTIONS

    OPEN INPUT Input-File
    PERFORM UNTIL End-Of-File="Y"
        READ Input-File
            AT END MOVE "Y" TO End-Of-File
            NOT AT END
                ADD 1 TO Row-Count
                PERFORM VARYING Idx FROM 1 BY 1 UNTIL (Idx > 200)
                    MOVE Input-Row(Idx:1)
                        TO Grid-Character(Row-Count,Idx)
                    IF (Input-Row(Idx:1) = " ")
                        EXIT PERFORM
                    END-IF
                END-PERFORM
                
                IF (Row-Count = 1)
                    SUBTRACT 1 FROM Idx
                    MOVE Idx TO Col-Count
                END-IF
        END-READ
    END-PERFORM
    CLOSE Input-File

    PERFORM COUNT-STRING-MATCHES
    DISPLAY Matches-Found

    PERFORM COUNT-X-MAS
    DISPLAY Matches-Found
    
    STOP RUN.

INIT-SEARCH-DIRECTIONS.
    *> Initializes Search DY and DX to be tables of relative directions to add to rows and columns
    MOVE 1 TO Idx
    PERFORM VARYING S-Idx1 FROM -1 BY 1 UNTIL (S-Idx1 > 1)
        PERFORM VARYING S-Idx2 FROM -1 BY 1 UNTIL (S-Idx2 > 1)
            IF (NOT (S-Idx1 = 0)) OR (NOT (S-Idx2 = 0))
                MOVE S-Idx1 TO Search-DX(Idx)
                MOVE S-Idx2 TO Search-DY(Idx)
                ADD 1 TO Idx
            END-IF
        END-PERFORM
    END-PERFORM.
    
COUNT-STRING-MATCHES. 
    *> Searches for the string saved in Search-String
    *> Assumes Search-Length is set to the length of Search-String
    MOVE 0 TO Matches-Found
    
    PERFORM VARYING Start-Row FROM 1 BY 1 UNTIL (Start-Row > Row-Count)
        PERFORM VARYING Start-Col FROM 1 BY 1 UNTIL (Start-Col > Col-Count)
            PERFORM VARYING Dir FROM 1 BY 1 UNTIL (Dir > 8)
                MOVE 'Y' TO Found-Match
                PERFORM VARYING Idx FROM 1 BY 1 UNTIL (Idx > Search-Length)
                    COMPUTE Current-Row = (Start-Row + Search-DX(Dir) * (Idx - 1))
                    COMPUTE Current-Col = (Start-Col + Search-DY(Dir) * (Idx - 1))
                    IF (Current-Row < 1) OR (Current-Row > Row-Count) OR (Current-Col < 1) OR (Current-Row > Row-Count)
                        MOVE 'N' TO Found-Match
                        EXIT PERFORM
                    END-IF
                    IF NOT (Search-String(Idx:1) = Grid-Character(Current-Row, Current-Col))
                        MOVE 'N' TO Found-Match
                        EXIT PERFORM
                    END-IF
                END-PERFORM

                IF (Found-Match = 'Y') AND (Idx > Search-Length)
                    ADD 1 TO Matches-Found
                END-IF
            END-PERFORM
        END-PERFORM
    END-PERFORM.
    
COUNT-X-MAS.
    MOVE 0 TO Matches-Found

    PERFORM VARYING Start-Row FROM 2 BY 1 UNTIL (Start-Row >= Row-Count)
        PERFORM VARYING Start-Col FROM 2 BY 1 UNTIL (Start-Col >= Col-Count)
            IF Grid-Character(Start-Row, Start-Col) = "A"
                MOVE 'Y' TO Found-Match
                COMPUTE Current-Row = Start-Row - 1;
                COMPUTE Current-Col = Start-Col - 1;
                IF Grid-Character(Current-Row, Current-Col) = 'M'
                    COMPUTE Current-Row = Start-Row + 1;
                    COMPUTE Current-Col = Start-Col + 1;
                    IF NOT (Grid-Character(Current-Row, Current-Col) = 'S')
                        MOVE 'N' TO Found-Match
                    END-IF
                ELSE
                    IF Grid-Character(Current-Row, Current-Col) = 'S'
                        COMPUTE Current-Row = Start-Row + 1;
                        COMPUTE Current-Col = Start-Col + 1;
                        IF NOT (Grid-Character(Current-Row, Current-Col) = 'M')
                            MOVE 'N' TO Found-Match
                        END-IF
                    ELSE
                        MOVE 'N' TO Found-Match
                    END-IF
                END-IF

                COMPUTE Current-Row = Start-Row + 1;
                COMPUTE Current-Col = Start-Col - 1;
                IF Grid-Character(Current-Row, Current-Col) = 'M'
                    COMPUTE Current-Row = Start-Row - 1;
                    COMPUTE Current-Col = Start-Col + 1;
                    IF NOT (Grid-Character(Current-Row, Current-Col) = 'S')
                        MOVE 'N' TO Found-Match
                    END-IF
                ELSE
                    IF Grid-Character(Current-Row, Current-Col) = 'S'
                        COMPUTE Current-Row = Start-Row - 1;
                        COMPUTE Current-Col = Start-Col + 1;
                        IF NOT (Grid-Character(Current-Row, Current-Col) = 'M')
                            MOVE 'N' TO Found-Match
                        END-IF
                    ELSE
                        MOVE 'N' TO Found-Match
                    END-IF
                END-IF

                IF Found-Match = 'Y'
                    ADD 1 TO Matches-Found
                END-IF
            END-IF
        END-PERFORM
    END-PERFORM.
