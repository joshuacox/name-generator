with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;
with Ada.Directories;

procedure Name_Generator_Ada is
   package Env renames Ada.Environment_Variables;
   package TIO renames Ada.Text_IO;
   package Dirs renames Ada.Directories;
   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;
   use type Dirs.File_Kind;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => SU.Unbounded_String);

   package Random_Index is new Ada.Numerics.Discrete_Random (Result_Subtype => Positive);

   function Get_Env_Or_Default (Key : String; Default : String) return String is
   begin
      if Env.Exists (Key) and then Env.Value (Key)'Length > 0 then
         return Env.Value (Key);
      else
         return Default;
      end if;
   end Get_Env_Or_Default;

   function Pick_Random_File (Folder : String; Gen : in out Random_Index.Generator) return String is
      Search : Dirs.Search_Type;
      Item   : Dirs.Directory_Entry_Type;
      Files  : String_Vectors.Vector;
   begin
      if Dirs.Exists (Folder) and then Dirs.Kind (Folder) = Dirs.Directory then
         Dirs.Start_Search (Search, Folder, "*", (Dirs.Ordinary_File => True, others => False));
         while Dirs.More_Entries (Search) loop
            Dirs.Get_Next_Entry (Search, Item);
            Files.Append (SU.To_Unbounded_String (Dirs.Full_Name (Item)));
         end loop;
         Dirs.End_Search (Search);
      end if;

      if Files.Is_Empty then
         TIO.Put_Line (TIO.Standard_Error, "Folder `" & Folder & "` contains no regular files.");
         return "";
      end if;

      declare
         Idx : constant Positive := (Random_Index.Random (Gen) mod Positive (Files.Length)) + 1;
      begin
         return SU.To_String (Files.Element (Idx));
      end;
   end Pick_Random_File;

   function Read_Lines (File_Path : String; Lowercase : Boolean) return String_Vectors.Vector is
      File  : TIO.File_Type;
      Vec   : String_Vectors.Vector;
      Line  : String (1 .. 2048);
      Last  : Natural;
   begin
      TIO.Open (File, TIO.In_File, File_Path);
      while not TIO.End_Of_File (File) loop
         TIO.Get_Line (File, Line, Last);
         declare
            Raw : constant String := Line (1 .. Last);
            First_Idx : Positive := 1;
            Last_Idx  : Natural := Last;
         begin
            while First_Idx <= Last_Idx and then (Raw (First_Idx) = ' ' or Raw (First_Idx) = ASCII.HT) loop
               First_Idx := First_Idx + 1;
            end loop;
            while Last_Idx >= First_Idx and then (Raw (Last_Idx) = ' ' or Raw (Last_Idx) = ASCII.HT or Raw (Last_Idx) = ASCII.CR) loop
               Last_Idx := Last_Idx - 1;
            end loop;

            if First_Idx <= Last_Idx then
               declare
                  Trimmed : constant String := Raw (First_Idx .. Last_Idx);
               begin
                  if Lowercase then
                     Vec.Append (SU.To_Unbounded_String (Ada.Characters.Handling.To_Lower (Trimmed)));
                  else
                     Vec.Append (SU.To_Unbounded_String (Trimmed));
                  end if;
               end;
            end if;
         end;
      end loop;
      TIO.Close (File);
      return Vec;
   end Read_Lines;

   Gen          : Random_Index.Generator;
   Separator    : constant String := Get_Env_Or_Default ("SEPARATOR", "-");
   Noun_Folder  : constant String := Get_Env_Or_Default ("NOUN_FOLDER", "nouns");
   Adj_Folder   : constant String := Get_Env_Or_Default ("ADJ_FOLDER", "adjectives");
   Noun_File    : SU.Unbounded_String;
   Adj_File     : SU.Unbounded_String;
   Counto_Str   : constant String := Get_Env_Or_Default ("counto", "24");
   Counto       : Positive := 24;
   Is_Debug     : constant Boolean := (Get_Env_Or_Default ("DEBUG", "false") = "true");

   Nouns        : String_Vectors.Vector;
   Adjectives   : String_Vectors.Vector;
begin
   Random_Index.Reset (Gen);

   if Env.Exists ("NOUN_FILE") and then Env.Value ("NOUN_FILE")'Length > 0 then
      Noun_File := SU.To_Unbounded_String (Env.Value ("NOUN_FILE"));
   else
      Noun_File := SU.To_Unbounded_String (Pick_Random_File (Noun_Folder, Gen));
   end if;

   if Env.Exists ("ADJ_FILE") and then Env.Value ("ADJ_FILE")'Length > 0 then
      Adj_File := SU.To_Unbounded_String (Env.Value ("ADJ_FILE"));
   else
      Adj_File := SU.To_Unbounded_String (Pick_Random_File (Adj_Folder, Gen));
   end if;

   begin
      Counto := Positive'Value (Counto_Str);
   exception
      when others =>
         Counto := 24;
   end;

   Nouns := Read_Lines (SU.To_String (Noun_File), True);
   Adjectives := Read_Lines (SU.To_String (Adj_File), False);

   if Nouns.Is_Empty or Adjectives.Is_Empty then
      return;
   end if;

   for I in 0 .. Counto - 1 loop
      declare
         N_Idx : constant Positive := (Random_Index.Random (Gen) mod Positive (Nouns.Length)) + 1;
         A_Idx : constant Positive := (Random_Index.Random (Gen) mod Positive (Adjectives.Length)) + 1;
         Noun  : constant String := SU.To_String (Nouns.Element (N_Idx));
         Adj   : constant String := SU.To_String (Adjectives.Element (A_Idx));
      begin
         if Is_Debug then
            TIO.Put_Line (TIO.Standard_Error, Adj);
            TIO.Put_Line (TIO.Standard_Error, Noun);
            TIO.Put_Line (TIO.Standard_Error, SU.To_String (Adj_File));
            TIO.Put_Line (TIO.Standard_Error, Adj_Folder);
            TIO.Put_Line (TIO.Standard_Error, SU.To_String (Noun_File));
            TIO.Put_Line (TIO.Standard_Error, Noun_Folder);
            TIO.Put_Line (TIO.Standard_Error, Integer'Image (I) & " > " & Integer'Image (Counto));
         end if;

         TIO.Put_Line (Adj & Separator & Noun);
      end;
   end loop;
end Name_Generator_Ada;
