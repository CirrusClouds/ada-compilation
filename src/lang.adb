with Ada.Text_IO;   use Ada.Text_IO;
with Interfaces.C;

procedure Lang is
   package C renames Interfaces.C;

   type Oper is (Plus, Dump, Minus, Push, No_op);

   type Idk is record
      operation  : Oper;
      value  : Integer;
   end record;

   type Prog is array (Positive range 1..1000) of Idk;
   
   type Stack is array (Positive range 1..100) of Integer;

   function system(command: C.char_array) return C.int
      with Import, Convention => C;
   

   function push_op (num : Integer) return Idk is
   begin
      return (operation => Push, value => num);
   end;

   function plus_op return Idk is
   begin
      return (operation => Plus, value => 0);
   end;

   function minus_op return Idk is
   begin
      return (operation => Minus, value => 0);
   end;

   function dump_op return Idk is
   begin
      return (operation => Dump, value => 0); 
   end;

   procedure Interpret (Program : Prog) is
   counter : Natural := 0;
   store : Stack := (others => 0);
   I : Natural := 1;
   a : Integer;
   b : Integer;

   function pop_stack return Integer is
   popped : Natural;
   begin
      popped := store(counter);
      store(counter) := 0;
      counter := counter - 1;
      return popped;
   end;

   procedure push_stack (num : Integer) is
   begin
      counter := counter + 1;
      store(counter) := num;
   end;

   procedure dump_stack is
   I : Natural := 1;
   begin
      while I <= counter loop
         Put_line(store(I)'Image);
         I := I + 1;
      end loop;
   end;

   begin
      for I in Program'Range loop
         case Program(I).operation is
            when Push =>
               push_stack (Program(I).value);
            when Plus =>
               a := pop_stack;
               b := pop_stack;
               push_stack (a+b);
            when Minus =>
               a := pop_stack;
               b := pop_stack;
               push_stack (b-a);
            when Dump =>
               dump_stack;
            when No_op =>
               exit;
         end case;   
      end loop;
   end;

   

   procedure Compile (Program : Prog) is
   F : File_Type;
   File_name : String := "obj/out.asm";
   I : Natural := 1;
   sysreturn : C.Int;
   
   begin
      Create (F, Out_File, File_Name);
      Put_Line (F, "section .text");
      Put_Line (F, "   extern prints");
      Put_Line (F, "   global main");
   
      Put_Line (F, "dump:");
      Put_Line (F, "   call prints");
      Put_Line (F, "   ret");
   
      Put_Line (F, "main:");
   
      for I in Program'Range loop
         case Program(I).operation is
            when Push =>
               Put_Line (F, "   push" & Program(I).value'Image);
            when Plus =>
               Put_Line(F,  "   pop rax");
               Put_Line(F,  "   pop rbx");
               Put_Line(F,  "   add rax, rbx");
               Put_Line(F,  "   push rax");
            when Minus =>
               Put_Line(F,  "   pop rax");
               Put_Line(F,  "   pop rbx");
               Put_Line(F,  "   sub rax, rbx");
               Put_Line(F,  "   push rax");
            when Dump =>
               Put_Line (F, "   call dump");
            when No_op =>
               exit;
         end case;
      end loop;

      Put_Line (F, "   mov rax, 60"); 
      Put_Line (F, "   mov rdi, 0");
      Put_Line (F, "   syscall"); 
      Close (F);
      sysreturn := system ("nasm -felf64 obj/out.asm -o obj/out.o && nasm -felf64 src/assembly/printf.asm -o src/assembly//printf.o && gcc -o obj/out src/assembly/printf.o obj/out.o");
   
   end;
   
   
   Program : Prog := (push_op(5), push_op(12), plus_op, dump_op, others => (operation=>No_op, value=>0));

   result : C.int;
   
begin
   -- result := system(Command);
   Interpret (Program);
   Compile (Program);

end Lang;   
