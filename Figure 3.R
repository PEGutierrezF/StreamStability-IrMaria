
library(DiagrammeR) ; library(DiagrammeRsvg) ; library(rsvg) ; require(tiff)


plot <- grViz("
digraph boxes_and_circles{

  # a 'graph' statement with increased margin
  graph [overlap = false]

  # first subgraph (diagram 1)
    subgraph cluster_1 {
        fontsize = 18;fontname = Arial;
        label = 'A)';
        color=white;
        labelloc = t;
        labeljust = l;
    
  
    node [shape = box, fontname = Arial]
    'Canopy openness'; 'Leaf litter'; 'Epilithic algae';
    'Decapoda abundance'; 'Macroinvertebrate\ndensity'

    # several 'edge' statements with numbers
    edge [minlen = 1, arrowhead=vee]
    'Canopy openness' -> 'Leaf litter' [label = '-0.39\n(-0.63; -0.15)', fontsize = 12]
    'Canopy openness' -> 'Epilithic algae' [label = '-0.97\n   (-1.62; -0.47)', fontsize = 12]
    'Leaf litter' -> 'Decapoda abundance' [label = 'N.E.', style = dotted, fontsize = 12]
    'Epilithic algae' -> 'Decapoda abundance' [label = '-1.00\n   (-1.55; -0.55)', fontsize = 12]
    'Decapoda abundance' -> 'Epilithic algae' [label = '1.08\n(0.26; 2.17)', fontsize = 12]
    'Leaf litter' -> 'Macroinvertebrate\ndensity' [label = 'N.E.', style = dotted, fontsize = 12]
    'Epilithic algae' -> 'Macroinvertebrate\ndensity' [label = 'N.E.', style = dotted, fontsize = 12]
    'Decapoda abundance' -> 'Macroinvertebrate\ndensity' [label = '  N.E.', style = dotted, fontsize = 12]

    { rank = same; 'Leaf litter'; 'Epilithic algae'; }
}  //for the Squared

  # second subgraph (diagram 2)
    subgraph cluster_2 {
        fontsize = 18;fontname = Arial;
        label = 'B)';
        color=white;
        labelloc = t;
        labeljust = l;
    
        
    node [shape = box, fontname = Arial]
    'Canopy openness '; 'Leaf litter '; 'Epilithic algae ';
    'Decapoda abundance '; 'Macroinvertebrate\ndensity '

    # several 'edge' statements with numbers
    edge [minlen = 1, arrowhead=vee]
    'Canopy openness ' -> 'Leaf litter ' [label = 'N.E.', style = dotted, fontsize = 12]
    'Canopy openness ' -> 'Epilithic algae ' [label = '-0.91\n   (-1.39; -0.55)', fontsize = 12]
    'Leaf litter ' -> 'Decapoda abundance ' [label = 'N.E.', style = dotted, fontsize = 12]
    'Epilithic algae ' -> 'Decapoda abundance ' [label = '-0.98\n(-1.57; -0.51)', fontsize = 12]
    'Decapoda abundance ' -> 'Epilithic algae ' [label = '0.78\n(0.17; 1.68)', fontsize = 12]
    'Leaf litter ' -> 'Macroinvertebrate\ndensity ' [label = 'N.E.', style = dotted, fontsize = 12]
    'Epilithic algae ' -> 'Macroinvertebrate\ndensity ' [label = 'N.E.', style = dotted, fontsize = 12]
    'Decapoda abundance ' -> 'Macroinvertebrate\ndensity ' [label = '  N.E.', style = dotted, fontsize = 12]

    { rank = same; 'Leaf litter '; 'Epilithic algae '; }

} //for the Squared
}
")
plot

export_svg(plot) %>% 
  charToRaw %>% 
  rsvg(width = 10000, 
       height = 6000) %>% 
  tiff::writeTIFF("Figure 3a.tiff", bits.per.sample = 8L)


# Ensure the subfolder 'figures' exists
if(!dir.exists("figures")) {
  dir.create("figures")
}

# Export the plot as a TIFF file in the 'figures' subfolder
export_svg(plot) %>% 
  charToRaw %>% 
  rsvg(width = 10000, height = 6000) %>% 
  tiff::writeTIFF("figures/Figure_3a.tiff", bits.per.sample = 8L)
