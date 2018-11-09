This is a collection of utilities that I've found helpful while
working with pollen.

- `pollen-extensions/latex-base` : This exports, among other things, a
  `->ltx` function which can transform the following tags into latex
  source:
    - macro
    - environment
    - l (unordered lists)
    - ol (ordered lists)
    - eql (lists of equations)
- pollen-extensions/utility : This exports some utilities that I've
  found helpful, partly for finding errors, partly for decoding
  textual information into pollen markup. I prefer to have pollen to
  more of the heavy lifting of figuring out what I mean, rather than
  making the markup itself more detailed.
