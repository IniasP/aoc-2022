
Program AoC11;

Uses 
SysUtils, Classes, regexpr, StrUtils;

Type 
  TIntArray = array Of int64;
  TBinOperation = (add, sub, mul);
  TOperandType = (old, num);
  TOperand = Record
    Case operandType : TOperandType Of 
      old: ();
      num: (val: int64);
  End;
  TBinExpr = Record
    operation : TBinOperation;
    left : TOperand;
    right : TOperand;
  End;
  TMonkey = Record
    items : TIntArray;
    expr: TBinExpr;
    testNum, testTrueMonkey, testFalseMonkey : int64;
  End;

Var 
  i: Integer;
  lineInMonkey: Integer;
  line: string;
  f: TextFile;
  monkeys: array [0..10] Of TMonkey;
  monkeyNum: Integer;
  operationExpr: TRegExpr;
Function ParseItems(str: String): TIntArray;

Var 
  items: TIntArray;
  i: Integer;
  itemStr: TStringArray;
Begin
  itemStr := SplitString(str, '  Starting items: ')[1].Split([', ']);
  SetLength(items, Length(itemStr));
  For i := 0 To Length(itemStr) - 1 Do
    Begin
      items[i] := StrToInt(itemStr[i]);
    End;
  ParseItems := items;
End;
Function ItemsToString(arr: TIntArray): String;

Var 
  result: String;
Begin
  result := '';
  For i := 0 To Length(arr) - 1 Do
    Begin
      result := result + IntToStr(arr[i]) + ' ';
    End;
  ItemsToString := result;
End;
Function ExpressionToString(expr: TBinExpr): String;

Var 
  result: String;
Begin
  Case expr.left.operandType Of 
    old: result := 'old';
    num: result := IntToStr(expr.left.val);
  End;
  Case expr.operation Of 
    add: result := result + ' + ';
    sub: result := result + ' - ';
    mul: result := result + ' * ';
  End;
  Case expr.right.operandType Of 
    old: result := result + 'old';
    num: result := result + IntToStr(expr.right.val);
  End;
  ExpressionToString := result;
End;
Function ParseOperand(str: String): TOperand;
Begin
  If (str = 'old') Then
    ParseOperand.operandType := old
  Else
    Begin
      ParseOperand.operandType := num;
      ParseOperand.val := StrToInt(str);
    End;
End;
Function ParseBinOperation(str: String): TBinOperation;
Begin
  Case str Of 
    '+': ParseBinOperation := add;
    '-': ParseBinOperation := sub;
    '*': ParseBinOperation := mul;
  End;
End;
Begin
  (* parse monkeys from input *)
  assign(f, 'input_example');
  reset(f);
  lineInMonkey := 0;
  monkeyNum := 0;
  While Not EOF(f) Do
    Begin
      operationExpr := TRegExpr.Create;
      operationExpr.Expression := '  Operation: new = (old|\d+) ([-+*]) (old|\d+)';
      readLn(f, line);
      Case (lineInMonkey) Of 
        1:
           Begin
             writeln('monkey ', monkeyNum);
             monkeys[monkeyNum].items := ParseItems(line);
             write('starting items: ');
             writeln(ItemsToString(monkeys[monkeyNum].items));
           End;
        2:
           Begin
             operationExpr.Exec(line);
             monkeys[monkeyNum].expr.left := ParseOperand(operationExpr.Match[1]);
             monkeys[monkeyNum].expr.right := ParseOperand(operationExpr.Match[3]);
             monkeys[monkeyNum].expr.operation := ParseBinOperation(operationExpr.Match[2]);
             write('new value: ');
             writeln(ExpressionToString(monkeys[monkeyNum].expr));
           End;
        3:
           Begin
             monkeys[monkeyNum].testNum := StrToInt(SplitString(line, '  Test: divisible by ')[1]);
             write('divisable test: ');
             writeln(monkeys[monkeyNum].testNum);
           End;
        4:
           Begin
             monkeys[monkeyNum].testTrueMonkey := StrToInt(SplitString(line, '    If true: throw to monkey ')[1]);
             write('true: ');
             writeln(monkeys[monkeyNum].testTrueMonkey);
           End;
        5:
           Begin
             monkeys[monkeyNum].testFalseMonkey := StrToInt(SplitString(line, '    If false: throw to monkey ')[1]);
             write('false: ');
             writeln(monkeys[monkeyNum].testFalseMonkey);
           End;
      End;
      If (lineInMonkey = 6) Then
        Begin
          writeln('-----');
          lineInMonkey := 0;
          monkeyNum := monkeyNum + 1;
        End
      Else
        Begin
          lineInMonkey := lineInMonkey + 1;
        End;
    End;
  close(f);
  writeln('=====');
  writeln('parsed ', monkeyNum + 1, ' monkeys')
  (* run the puzzle *)

End.
