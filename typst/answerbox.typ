#let answerbox(height: 2in, label: none) = {
  block[
    #if label != none [
      *#label*
      #v(4pt)
    ]
    #box(
      width: 100%,
      height: height,
      inset: 8pt,
      stroke: .5pt,
    )
  ]
}