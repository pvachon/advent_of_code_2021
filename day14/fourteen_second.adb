with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Containers.Hashed_Sets,
     Ada.Containers.Ordered_Sets,
     Ada.Strings.Hash;

procedure Fourteen_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    Nr_Iterations : constant Natural := 40;

    type Mapping_Count is record
        monomer : Character;
        count : Long_Long_Integer;
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

    type Memoization is record
        pair : String(1..2);
        depth : Natural;
        counts : Mapping_Counts.Set;
    end record;

    function "<"(Left, Right : in Memoization) return Boolean is begin
        if Left.pair < Right.pair then
            Return True;
        elsif Left.pair = Right.pair then
            if Left.depth < Right.depth then
                return True;
            end if;
        end if;
        return False;
    end "<";

    function "="(Left, Right : in Memoization) return Boolean is begin
        return Left.depth = Right.depth and Left.pair = Right.pair;
    end "=";

    package Memoizations is new Ada.Containers.Ordered_Sets(
            Element_Type => Memoization
        );
    memo : Memoizations.Set;

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

    procedure Parse_Mapping(line : String) is begin
        if line = "" then
            return;
        end if;
        mappings.Insert((fragment => line(line'First..line'First+1), mapping => line(line'Last), others => <>));
    end Parse_Mapping;

    procedure Find_Counts(template : String) is
        use Chain_Mappings;

        procedure Update_Count(monomer : in Character;
                               counts : in out Mapping_Counts.Set;
                               nr_items : in Long_Long_Integer := 1) is
            use Mapping_Counts;
            c : Mapping_Counts.Cursor := counts.Find((monomer => monomer, others => <>));
        begin
            if c /= Mapping_Counts.No_Element then
                counts.Replace((monomer => monomer, count => Mapping_Counts.Element(c).count + nr_items));
            else
                counts.Insert((monomer => monomer, count => nr_items));
            end if;
        end Update_Count;

        procedure Combine_Counts(out_counts : in out Mapping_Counts.Set;
                                 tmp_counts : in Mapping_Counts.Set) is

            use Mapping_Counts;
        begin
            for J of tmp_counts loop
                Update_Count(J.monomer, out_counts, J.count);
            end loop;
        end Combine_Counts;

        type State is array(1..2) of Character;

        function Get_Monomer(st : in State;
                             depth : in Natural;
                             counts : in out Mapping_Counts.Set;
                             counted : out Boolean) return Character
        is
            use Memoizations;
            c : Chain_Mappings.Cursor := mappings.Find((fragment => (st(1), st(2)), others => <>));
            mc : Memoizations.Cursor := memo.Find((depth => depth, pair => (st(1), st(2)), others => <>));
        begin
            counted := False;

            if c = Chain_Mappings.No_Element then
                Text_IO.Put_Line("Missing mapping for fragment!");
                raise Ada.Strings.Index_Error;
            end if;

            -- See if we already have memoized for this depth.
            if mc /= Memoizations.No_element then
                counted := True;
                counts := Element(mc).counts.Copy;
            end if;

            return Element(c).mapping;
        end Get_Monomer;

        procedure Record_Counts(pair        : in State;
                                depth       : in Natural;
                                in_counts   : in out Mapping_Counts.Set)
        is
            use Memoizations;
            mc : Memoizations.Cursor := memo.Find((pair => (pair(1), pair(2)), depth => depth, others => <>));
        begin
            if mc = Memoizations.No_Element then
                memo.Insert((pair => (pair(1), pair(2)), depth => depth, counts => in_counts.Copy));
            end if;
        end Record_Counts;

        procedure Step(st           : in State;
                       count        : in Natural;
                       step_counts  : in out Mapping_Counts.Set)
        is
            got_count : Boolean := false;
            monomer : Character := Get_Monomer((st(1), st(2)), count, step_counts, got_count);
            count_l, count_r : Mapping_Counts.Set;
        begin
            if count = Nr_Iterations then
                Update_Count(st(1), step_counts, 1);
                return;
            end if;

            if got_count then
                return;
            end if;

            Step((st(1), monomer), count + 1, count_l);
            Combine_Counts(step_counts, count_l);
            Step((monomer, st(2)), count + 1, count_r);
            Combine_Counts(step_counts, count_r);
            Record_Counts(st, count, step_counts);
        end;

    begin
        for J in template'First..template'Last - 1 loop
            Template_Iter : declare
                step_counts : Mapping_Counts.Set;
            begin
                Step((template(J), template(J+1)), 0, step_counts);
                Combine_Counts(all_counts, step_counts);
            end Template_Iter;
        end loop;
        Update_Count(template(template'Last), all_counts, 1);
    end Find_Counts;
begin

    Text_IO.Put_Line("Advent of Code 2021: 14.1");

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
        max : Long_Long_Integer := 0;
        min : Long_Long_Integer := Long_Long_Integer'Last;
    begin
        for count of all_counts loop
            max := (if max < count.count then count.count else max);
            min := (if min > count.count then count.count else min);
        end loop;

        Text_IO.Put_Line("Most common: " & Long_Long_Integer'Image(max) &
            ", least: " & Long_Long_Integer'Image(min) &
            " difference: " & Long_Long_Integer'Image(max - min));
    end Find_Extrema;

end Fourteen_Second;
