#set page(
  header-ascent: 0% + .2in,
  header: context {
    if counter(page).get().first() == 1 {
      block[
        #grid(
          columns: (1fr, auto),
          rows: (0.45in),
          align: horizon,  // vertical centering across the row
          align(left)[
            #grid(
              columns: (auto, auto),
              column-gutter: 6pt,
              [Name:],
              box(
                width: 4in,
                height: 0.45in,
                stroke: (bottom: 1pt),
              ),
            )
          ],
          align(right)[
            #grid(
              columns: (auto, auto),
              column-gutter: 6pt,
              [LA Initials:],
              box(
                width: 0.65in,
                height: 0.45in,
                stroke: 1pt
              ),
            )
          ],
        )
      ]
    }
  },
)