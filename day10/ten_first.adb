with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Strings;

procedure Ten_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    function Check_Line(line : String) return Natural is
        package Stack is new Ada.Containers.Vectors(
            Element_Type => Character,
            Index_Type => Natural);

        st : Stack.Vector;

        procedure Push(elem : Character) is begin
            st.Append(New_Item => elem);
        end Push;

        function Pop return Character is
            char : Character := st.Last_Element;
        begin
            st.Delete_Last;
            return char;
        end Pop;

        function Score(elem : Character) return Natural is begin
            case elem is
                when ')' => return 3;
                when ']' => return 57;
                when '}' => return 1197;
                when '>' => return 25137;
                when others => raise Ada.Strings.Index_Error;
            end case;
        end Score;

        function Is_Matching(open, close : Character) return Boolean is begin
            return (open = '{' and close = '}') or
                    (open = '[' and close = ']') or
                    (open = '<' and close = '>') or
                    (open = '(' and close = ')');
        end Is_Matching;
    begin

        for C of line loop
            case C is
                when '{' | '[' | '<' | '(' => Push(C);
                when '}' | ']' | '>' | ')' =>
                    Close_Bracket: declare
                        expected : Character := Pop;
                    begin
                        if not Is_Matching(expected, C) then
                            Text_IO.Put_Line("For [" & line & "] got got a " & C & ", expected a " & expected);
                            return Score(C);
                        end if;
                    end Close_Bracket;
                when others => raise Ada.Strings.Index_Error;
            end case;
        end loop;

        return 0;
    end Check_Line;

    error_sum : Natural := 0;
begin

    Text_IO.Put_Line("Advent of Code 2021: 10.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        error_sum := error_sum + Check_Line(Text_IO.Get_Line(file));
    end loop;

    Text_IO.Put_Line("Error score is: " & Natural'Image(error_sum));

end Ten_First;
