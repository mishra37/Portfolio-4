# Portfolio-4

Link: https://mishra37.shinyapps.io/portfolio-4/

After working extremely hard on the 2nd Portfolio, my main motive was to deliver as much information as possible through my visualizations because understanding mass-shooting in different states is extremely crucial for the safety of the people. My motivation to improve this visualization came after receiving feedback from Prof. Sankaran, and peers and also because it was one of my best portfolios where I incorporated different visualizing techniques to convey the idea. 

I decided to first work on the layout of my visualizations by removing the excessive white space and making the three visualizations look more compact.  While doing this, I kept the Map visualization in the center and ensured that it occupied larger space compared to the rest two visualizations. I did this because I wanted to capture users’ attention more towards the map. Apart from that, I changed the color scheme for the map and PCA scores scatter plot by using non-diverging colors and only using different shades of red to signify total shooting causalities. Although, I used a reactive function to highlight the state on the map plot whenever a point was clicked on scatter plot by using scale_alpha, but to make it clearer, I decided to highlight the borders of the selected states as well. Moreover, I also got rid of the legend for scatter plot and kept the one for the map, since it was delivering the same information as the map plot. Also, for the time series (area plot), I changed the color scheme by only having different shades of blue. Lastly, I added a line of instruction in the title panel to make sure that after clicking on any point of the scatter plot, user can reset all the plots by double clicking on any empty area of the scatter plot. 

After making the above changes, all the visualizations in the shiny app looks more compact and captures high attention towards the map plot. It also does a decent job in relying the information about causalities by the use of different shades of red. 
