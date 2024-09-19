
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
    'Canopy openness' -> 'Leaf litter' [label = '-0.40\n(-0.64; 0.15)', fontsize = 12]
    'Canopy openness' -> 'Epilithic algae' [label = '-2.91\n   (-6.94; -0.85)', fontsize = 12]
    'Leaf litter' -> 'Decapoda abundance' [label = 'N.E.', style = dotted, fontsize = 12]
    'Epilithic algae' -> 'Decapoda abundance' [label = '-1.88\n   (-3.75; -0.94)', fontsize = 12]
    'Decapoda abundance' -> 'Epilithic algae' [label = '4.86\n(0.98; 12.03)', fontsize = 12]
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
    'Canopy openness ' -> 'Epilithic algae ' [label = '-1.73\n   (-4.62; -0.67)', fontsize = 12]
    'Leaf litter ' -> 'Decapoda abundance ' [label = '0.55\n(0.07;1.46)', fontsize = 12]
    'Epilithic algae ' -> 'Decapoda abundance ' [label = '-1.71\n(-3.72; -0.72)', fontsize = 12]
    'Decapoda abundance ' -> 'Epilithic algae ' [label = '2.68\n(0.41; 9.18)', fontsize = 12]
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
  tiff::writeTIFF("Figure 3.tiff", bits.per.sample = 8L)


# Ensure the subfolder 'figures' exists
if(!dir.exists("figures")) {
  dir.create("figures")
}

# Export the plot as a TIFF file in the 'figures' subfolder
export_svg(plot) %>% 
  charToRaw %>% 
  rsvg(width = 10000, height = 6000) %>% 
  tiff::writeTIFF("figures/Figure_2.tiff", bits.per.sample = 8L)
