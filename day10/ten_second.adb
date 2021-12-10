with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Strings;

procedure Ten_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    function Check_Line(line : String) return Long_Long_Integer is
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

        function Score(elem : Character) return Long_Long_Integer is begin
            case elem is
                when '(' =>
                    Text_IO.Put(")");
                    return 1;
                when '[' =>
                    Text_IO.Put("]");
                    return 2;
                when '{' =>
                    Text_IO.Put("}");
                    return 3;
                when '<' =>
                    Text_IO.Put(">");
                    return 4;
                when others => raise Ada.Strings.Index_Error;
            end case;
        end Score;

        function Is_Matching(open, close : Character) return Boolean is begin
            return (open = '{' and close = '}') or
                    (open = '[' and close = ']') or
                    (open = '<' and close = '>') or
                    (open = '(' and close = ')');
        end Is_Matching;

        total_score : Long_Long_Integer := 0;
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
                            -- Skip trying to process this line
                            return 0;
                        end if;
                    end Close_Bracket;
                when others => raise Ada.Strings.Index_Error;
            end case;
        end loop;

        Text_IO.Put("For [" & line & "] there are " & Ada.Containers.Count_Type'Image(st.Length) & " elements -> ");
        while not st.Is_Empty loop
            total_score := total_score * 5 + Score(Pop);
        end loop;
        Text_IO.Put_Line(" Score: " & Long_Long_Integer'Image(total_score));

        return total_score;
    end Check_Line;

    package Scores is new Ada.Containers.Vectors(
        Element_Type => Long_Long_Integer,
        Index_Type => Natural);
    score_list : Scores.Vector;

    package Score_Sorter is new Scores.Generic_Sorting;

    score : Long_Long_Integer;
begin

    Text_IO.Put_Line("Advent of Code 2021: 10.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        score := Check_Line(Text_IO.Get_Line(file));
        if 0 /= score then
            score_list.Append(score);
        end if;
    end loop;

    Score_Sorter.Sort(score_list);

    Text_IO.Put_Line("Error score is: " & Long_Long_Integer'Image(score_list(score_list.Last_Index/2)));

end Ten_Second;
