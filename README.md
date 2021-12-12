# Home Theater Calculator

A toy for finding idealized layouts for a Home Theater or Studio setup, written by a guy who is not really all that into home theater, and who also hasn't followed any of the advice suggested by the tool. 

<p align="center">
  <img src="https://d39wtn5cxihhz5.cloudfront.net/content/reflection-computation.gif"/>
</p>


I've never let small things like having literally no idea what I'm talking about stop me, so you can check out all my moral grandstanding about theater setups here: [Home Theater Calculator](https://chriskiehl.com/article/home-theater-calculator) 


### About 

This is mostly one of those "build something to learn something better" projects. In this case, [Purescript](https://www.purescript.org/), with which I am currently totally enamored. It should be taken as such. 

It was also a vehicle for me playing with pixel art. As a non-artist, I think we can all agree: I should stick to programming. 


### Running

```
yarn spago install 
yarn spago build
yarn run dev
```

### Bundling for deployment

```
yarn parcel build assets/index.js
```

copy/paste the output file into the blog dir. 


**Special Thanks:**

[Sandy Maguire](https://github.com/isovector) gave me an awesome and thorough [code review](https://github.com/chriskiehl/home-theater-calculator/pull/1).





