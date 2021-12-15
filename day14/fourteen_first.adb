with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Containers.Hashed_Sets,
     Ada.Strings.Hash;

procedure Fourteen_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    Nr_Iterations : constant Natural := 10;

    type Mapping is record
        fragment : String(1..2);
        mapping : Character;
    end record;

    function Hash(Element : in Mapping) return Ada.Containers.Hash_Type is begin
        return Ada.Strings.Hash(Element.fragment);
    end Hash;

    function "="(Left, Right : in Mapping) return Boolean is begin
        return Left.fragment = Right.fragment;
    end "=";

    package Chain_Mappings is new Ada.Containers.Hashed_Sets(
            Element_Type => Mapping,
            Hash => Hash,
            Equivalent_Elements => "="
        );
    mappings : Chain_Mappings.Set;

    type Mapping_Count is record
        monomer : Character;
        count : Natural;
    end record;

    function Hash(Element : in Mapping_Count) return Ada.Containers.Hash_Type is begin
        return Ada.Containers.Hash_Type(Character'Pos(Element.monomer));
    end Hash;

    function "="(Left, Right : in Mapping_Count) return Boolean is begin
        return Left.monomer = Right.monomer;
    end "=";

    package Mapping_Counts is new Ada.Containers.Hashed_Sets(
            Element_Type => Mapping_Count,
            Hash => Hash,
            Equivalent_Elements => "="
        );
    all_counts : Mapping_Counts.Set;

    procedure Parse_Mapping(line : String) is begin
        if line = "" then
            return;
        end if;
        mappings.Insert((fragment => line(line'First..line'First+1), mapping => line(line'Last)));
    end Parse_Mapping;

    procedure Find_Counts(template : String) is
        use Chain_Mappings;

        procedure Update_Count(monomer : in Character) is
            use Mapping_Counts;
            c : Mapping_Counts.Cursor := all_counts.Find((monomer => monomer, others => <>));
        begin
            if c /= Mapping_Counts.No_Element then
                all_counts.Replace((monomer => monomer, count => Mapping_Counts.Element(c).count + 1));
            else
                all_counts.Insert((monomer => monomer, count => 1));
            end if;
        end Update_Count;

        type State is array(1..2) of Character;

        function Get_Monomer(st : State) return Character is
            c : Cursor;
        begin
            c := mappings.Find((fragment => (st(1), st(2)), others => <>));
            if c = No_Element then
                Text_IO.Put_Line("Missing mapping for fragment!");
                raise Ada.Strings.Index_Error;
            end if;

            return Element(c).mapping;
        end Get_Monomer;

        procedure Step(st : State;
                       count : in Natural) is
            monomer : Character := Get_Monomer((st(1), st(2)));
        begin
            if count = Nr_Iterations then
                Update_Count(st(1));
                return;
            end if;

            Step((st(1), monomer), count + 1);
            Step((monomer, st(2)), count + 1);
        end;
    begin
        for J in template'First..template'Last - 1 loop
            Text_IO.Put_Line("Iteration: " & Natural'Image(J));
            Step((template(J), template(J+1)), 0);
        end loop;
        Update_Count(template(template'Last));
    end Find_Counts;
begin

    Text_IO.Put_Line("Advent of Code 2021: 14.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    -- Read in the template
    Do_It : declare
        template : String := Text_IO.Get_Line(file);
    begin
        while not Text_IO.End_Of_File(file) loop
            Parse_Mapping(Text_IO.Get_Line(file));
        end loop;

        -- Let's do this
        Text_IO.Put_Line("Template length: " & Natural'Image(template'Length));
        Find_Counts(template);
    end Do_It;

    Find_Extrema : declare
        max : Natural := 0;
        min : Natural := Natural'Last;
    begin
        for count of all_counts loop
            max := (if max < count.count then count.count else max);
            min := (if min > count.count then count.count else min);
        end loop;

        Text_IO.Put_Line("Most common: " & Natural'Image(max) & ", least: " & Natural'Image(min) &
            " difference: " & Natural'Image(max - min));
    end Find_Extrema;

end Fourteen_First;
