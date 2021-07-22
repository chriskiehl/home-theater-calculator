# home-theater-calculator

tl;dr: a wip toy for finding useful measurements related to home theater / studio setup. 


### Running

```
yarn spago install 
yarn spago build
yarn run dev
```

### Super quick overview for the review: 

This basically a glorified basic trig calculator that does two things: 

1. gives idealized theater / studio layouts for the parameters chosen (where "ideal" is defined by various FOV and audio config guidelines from THC and the ITU-R peeps)
2. Computes primary reflection points for those setups so you can nerd out and put up bass traps / transfusers / etc.. 


### Notable Modules: 

 * The `Types` module has all the, as you'd expect, types along with comments explaining what they do. I don't know of centralizing them all like this is normal, but I saw they did that in the [multipac](https://github.com/hdgarrood/multipac/) library, so I did the same
 * The `Core` module holds just about all the logic in the system. That's where you find all the movement handlers, collision detection, layout management, etc.. 

Some modules, like `Debugging` can be safely ignored for the most part. It's just quickly thrown together things for drawing debugging info onto the canvas. 

### State of the projcect: 

Pretty rough. After a ton of failed starts, attempts at building some highly cohesive compositional thing were abandoned in favor of getting it working. I spent a lot of time after the fact trying to work backwards into one, but results were not great. This kind of event driven 'game'(like) programming is pretty new to me, so the bulk of it turned into just bouncing things off the walls to see what would stick. 
   








