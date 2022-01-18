open Signal
open Belt

type cell = String(string) | Number(float)

type matrix<'a> = Arr.t<Arr.t<'a>>

type sheet = matrix<Ref.t<cell>>

let alphabet = [
  `A`,
  `B`,
  `C`,
  `D`,
  `E`,
  `F`,
  `G`,
  `H`,
  `I`,
  `J`,
  `K`,
  `L`,
  `M`,
  `N`,
  `O`,
  `P`,
  `Q`,
  `R`,
  `S`,
  `T`,
  `U`,
  `V`,
  `W`,
  `X`,
  `Y`,
  `Z`,
]

let colIndex = (col: string) => {
  let r = alphabet->Js.Array2.findIndex(x => x == col)
  if r == -1 {
    Result.Error(`Invalid column name: ${col}`)
  } else {
    Result.Ok(r)
  }
}

let indexToCol = (i: int): string => alphabet->Js.Array2.unsafe_get(i)

let parseNumber = x =>
  switch x->Float.fromString {
  | None => Result.Error(`expected any number, got ${x}`)
  | Some(x) => Result.Ok(x)
  }

let parseInt = x =>
  switch x->Int.fromString {
  | None => Result.Error(`expected int, got ${x}`)
  | Some(x) => Result.Ok(x)
  }

let tupled = (fa, fb) =>
  switch (fa, fb) {
  | (Result.Ok(a), Result.Ok(b)) => Result.Ok((a, b))
  | (Result.Error(a), _) => Result.Error(a)
  | (_, Result.Error(b)) => Result.Error(b)
  }

let getRes = (fa, fb) =>
  switch fa {
  | None => Result.Error(fb)
  | Some(a) => Result.Ok(a)
  }

let tryIndex = (fa, i, name, next) =>
  fa
  ->Arr.get(i)
  ->Sig.flatMap(o =>
    o
    ->Option.map(next)
    ->Option.getWithDefault(Sig.make(() => Result.Error(`${name} ${i->Int.toString} not found`)))
  )

let rgx = %re("/^(\d+)$/")
let expandValueToken = (state: sheet, x): Sig.t<Result.t<cell, string>> => {
  if Js.Re.test_(rgx, x) {
    Sig.make(() => parseNumber(x)->Result.map(y => Number(y)))
  } else {
    let hd = x->Js.String2.slice(~from=0, ~to_=1)
    let colI = colIndex(hd)
    let tl = x->Js.String2.sliceToEnd(~from=1)
    let rowI = parseInt(tl)

    switch tupled(colI, rowI) {
    | Result.Error(e) => Sig.make(() => Result.Error(e))
    | Result.Ok((c, r)) =>
      state->tryIndex(c, "col", rows =>
        rows->tryIndex(r, "row", r => r->Sig.map(x => Result.Ok(x)))
      )
    }
  }
}

let evalCell = (cell: cell) =>
  switch cell {
  | Number(x) => Result.Ok(x)
  | String(x) => Result.Error(`expected number, got string ${x}`)
  }

let expandEval = (state, x) => expandValueToken(state, x)->Sig.map(r => r->Result.flatMap(evalCell))

let evalExpr = (state: sheet, current): Sig.t<Result.t<float, string>> => {
  let eval = expandEval(state)
  switch current {
  | list{x} => eval(x)
  | list{"+", a, b} =>
    Sig.tuple(eval(a), eval(b))->Sig.map(((rl, rr)) =>
      tupled(rl, rr)->Result.map(((l, r)) => l +. r)
    )
  | _ =>
    Sig.make(() => Result.Error(
      `expected expression, got ${current->List.reduce("", (a, b) => `${a}${b}`)}`,
    ))
  }
}

let parse = (state: sheet, x: string): Sig.t<cell> => {
  let tokens = Js.String2.split(x, " ")->List.fromArray

  switch tokens {
  | list{"=", ...exprToks} =>
    evalExpr(state, exprToks)->Sig.map(x => {
      switch x {
      | Result.Ok(x) => Number(x)
      | Result.Error(e) => String(e)
      }
    })
  | _ => Sig.make(() => String(x))
  }
}

let show = c =>
  switch c {
  | String(x) => x
  | Number(x) => x->Float.toString
  }

@module("mobx")
external untracked: (@uncurry unit => 'a) => 'a = "untracked"

module Cell = {
  @react.component
  let make = Sig.component((~state: sheet, ~row: int, ~col: int) => {
    let isFocus = Ref.use(() => false)

    let inputState = Ref.use(() => "")

    let parsed = inputState->Sig.useFlatMap(x => parse(state, x))

    parsed->Sig.useEffect(x => {
      state->Arr.getUnsafe(col)->Sig.flatten->Arr.getUnsafe(row)->Sig.flatten->Ref.set(x)
      None
    })

    let display = Sig.useIfM(isFocus, inputState->Sig.useToSig, parsed->Sig.useMap(show))

    <input
      typeof="text"
      value={display->Sig.get}
      onChange={e => inputState->Ref.set(ReactEvent.Form.target(e)["value"])}
      onFocus={_ => isFocus->Ref.set(true)}
      onBlur={_ => isFocus->Ref.set(false)}
    />
  })
}

module Sheet = {
  let cols = 10
  let rows = 10

  let colRange = Array.range(0, cols)
  let rowRange = Array.range(0, rows)

  @react.component
  let make = Sig.component(() => {
    let sheetState: sheet = Arr.use(() => {
      let xs = colRange->Array.map(_ => Arr.make(rowRange->Array.map(_ => Ref.make(String("")))))
      xs
    })

    <table>
      <thead>
        <tr>
          <th />
          {colRange
          ->Array.map(col =>
            <th key={`head-${col->Int.toString}`}> {indexToCol(col)->React.string} </th>
          )
          ->React.array}
        </tr>
      </thead>
      <tbody>
        {rowRange
        ->Array.map(row => {
          <tr key={`row-body-${row->Int.toString}`}>
            <td> {row->Int.toString->React.string} </td>
            {colRange
            ->Array.map(col => {
              <td key={`${row->Int.toString}-${col->Int.toString}`}>
                <Cell.make state={sheetState} row={row} col={col} />
              </td>
            })
            ->React.array}
          </tr>
        })
        ->React.array}
      </tbody>
    </table>
  })
}

switch ReactDOM.querySelector("#root") {
| Some(elem) => ReactDOM.render(<React.StrictMode> <Sheet /> </React.StrictMode>, elem)
| None => ()
}
