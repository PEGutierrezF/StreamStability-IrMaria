
library(DiagrammeR)

plot <- grViz("
digraph boxes_and_circles{

  # a 'graph' statement
  graph [overlap = true, fontsize = 2]

  # first subgraph (diagram 1)
  # subgraph cluster_diagram_1 { //for the Squared
  
    node [shape = box, fontname = Helvetica]
    'Canopy openness'; 'Leaf litter'; 'Epilithic algae';
    'Decapoda abundance'; 'Macroinvertebrate\ndensity'

    # several 'edge' statements with numbers
    edge [minlen = 1, arrowhead=vee]
    'Canopy openness' -> 'Leaf litter' [label = '-0.40 (-0.64; 0.16)']
    'Canopy openness' -> 'Epilithic algae' [label = '-2.85 (-6.94; -0.83)']
    'Leaf litter' -> 'Decapoda abundance' [label = 'N.E.']
    'Epilithic algae' -> 'Decapoda abundance' [label = '-1.89\n(-3.59; -0.90)']
    'Decapoda abundance' -> 'Epilithic algae' [label = '4.73\n(0.91; 12.04)']
    'Leaf litter' -> 'Macroinvertebrate\ndensity' [label = 'N.E.']
    'Epilithic algae' -> 'Macroinvertebrate\ndensity' [label = 'N.E.']
    'Decapoda abundance' -> 'Macroinvertebrate\ndensity' [label = 'N.E.']

    { rank = same; 'Leaf litter'; 'Epilithic algae'; }
#  }  //for the Squared

  # second subgraph (diagram 2)
  # subgraph cluster_diagram_2          //for the Squared
  
    node [shape = box, fontname = Helvetica]
    'Canopy openness '; 'Leaf litter '; 'Epilithic algae ';
    'Decapoda abundance '; 'Macroinvertebrate\ndensity '

    # several 'edge' statements with numbers
    edge [minlen = 1, arrowhead=vee]
    'Canopy openness ' -> 'Leaf litter ' [label = 'N.E.']
    'Canopy openness ' -> 'Epilithic algae ' [label = '-1.72 (-4.61 ; -0.67)']
    'Leaf litter ' -> 'Decapoda abundance ' [label = '0.55\n(0.07;1.44)']
    'Epilithic algae ' -> 'Decapoda abundance ' [label = '-1.70\n(-3.72; -0.70)']
    'Decapoda abundance ' -> 'Epilithic algae ' [label = '2.66\n(0.39; 8.98)']
    'Leaf litter ' -> 'Macroinvertebrate\ndensity ' [label = 'N.E.']
    'Epilithic algae ' -> 'Macroinvertebrate\ndensity ' [label = 'N.E.']
    'Decapoda abundance ' -> 'Macroinvertebrate\ndensity ' [label = 'N.E.', style = dashed]

    { rank = same; 'Leaf litter '; 'Epilithic algae '; }

#} //for the Squared
}
")
plot


library(DiagrammeR) ; library(DiagrammeRsvg) ; library(rsvg) 
# export graph
export_svg(plot) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("simple_grv.jpeg")


