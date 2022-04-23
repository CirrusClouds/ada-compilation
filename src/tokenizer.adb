with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Tokenizer is
   type TokenType is (NULLTOKEN, SPEECHTOKEN, WORDTOKEN, NUMTOKEN, SEMICOLONTOKEN);

   type Token is record
      option : TokenType;
      start : Natural;
      finish : Natural;
      value : unbounded_string;
   end record;

   type TokenStore is array(Positive range 1..1000) of Token;

   Alpha_Set : Character_Set := To_Set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
   Digit_Set : Character_Set := To_Set("0123456789");
   Semicolon_Set : Character_Set := To_Set(";");
   Speech_Set : Character_Set := To_Set("""");
   Whitespace_Set : Character_Set := To_Set(" ");
   
   function Tokenize(S : String) return TokenStore is
      C : Natural := 1;
      I : Natural := 1;
      Token_Storage : TokenStore := (others => (option => NULLTOKEN,
                                                start => 0,
                                                finish => 0,
                                                value => To_Unbounded_String ("")));

      function Make_Token(token_type : TokenType; set : Character_Set) return Token is
         acc_token : Token := (option => token_type,
                                start => C,
                                finish => C,
                                value => To_Unbounded_String (""));
                                
         begin
            while Is_In(S(C), set) loop
               acc_token.finish := acc_token.finish + 1;
               acc_token.value := acc_token.value & S(C);
               C := C + 1;
            end loop;
            return acc_token;
         end;

      begin
         while C in S'Range loop
            if Is_In(S(C), Whitespace_Set) then -- skip whitespace
               C:= C + 1;
            elsif Is_In(S(C), Alpha_Set) then 
               Token_Storage(I) := Make_Token(WORDTOKEN, Alpha_Set);
               I := I + 1;
            elsif Is_In(S(C), Digit_Set) then
               Token_Storage(I) := Make_Token(NUMTOKEN, Digit_Set);
               I := I + 1;
            elsif Is_In(S(C), Semicolon_Set) then
               Token_Storage(I) := Make_Token(SEMICOLONTOKEN, Semicolon_Set);
               I := I + 1;
            elsif Is_In(S(C), Speech_Set) then
               Token_Storage(I) := Make_Token(SPEECHTOKEN, Speech_Set);
               I := I + 1;
            end if;
         end loop;
   
         return Token_Storage;
   
      end;

   store : TokenStore;
   I : Natural;

   begin
      Store := Tokenize("what can i say other than ""hello world""; ");  
      I := 1;
 
      while (Store(I).option /= NullToken) loop
         Put_Line(TokenType'Image (Store(I).option) & " " & To_String (Store(I).value));
         I := I + 1;
      end loop;
   
   end;
