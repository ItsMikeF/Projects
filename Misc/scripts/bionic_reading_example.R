library(httr)

url <- "https://bionic-reading1.p.rapidapi.com/convert"

payload <- "The Omega Team was the soul of the company. Whereas the rest of the
enterprise brought in the money to keep things going, by various commercial
applications of narrow AI, the Omega Team pushed ahead in their quest for what
had always been the CEO’s dream: building general artificial intelligence. Most
other employees viewed “the Omegas,” as they affectionately called them, as a
bunch of pie-in-the-sky dreamers, perpetually decades away from their goal.
They happily indulged them, however, because they liked the prestige that the
cutting-edge work of the Omegas gave their company, and they also appreciated
the improved algorithms that the Omegas occasionally gave them.
What they didn’t realize was that the Omegas had carefully crafted their
image to hide a secret: they were extremely close to pulling off the most
audacious plan in human history. Their charismatic CEO had handpicked them
not only for being brilliant researchers, but also for ambition, idealism and a
strong commitment to helping humanity. He reminded them that their plan was
extremely dangerous, and that if powerful governments found out, they would
do virtually anything—including kidnapping—to shut them down or, preferably,
to steal their code. But they were all in, 100%, for much the same reason that
many of the world’s top physicists joined the Manhattan Project to develop
nuclear weapons: they were convinced that if they didn’t do it first, someone less
idealistic would.
The AI they had built, nicknamed Prometheus, kept getting more capable.
Although its cognitive abilities still lagged far behind those of humans in many
areas, for example, social skills, the Omegas had pushed hard to make it
extraordinary at one particular task: programming AI systems. They’d
deliberately chosen this strategy because they had bought the intelligence
explosion argument made by the British mathematician Irving Good back in
1965: “Let an ultraintelligent machine be defined as a machine that can far
surpass all the intellectual activities of any man however clever. Since the design
of machines is one of these intellectual activities, an ultraintelligent machine
could design even better machines; there would then unquestionably be an
‘intelligence explosion,’ and the intelligence of man would be left far behind.
Thus the first ultraintelligent machine is the last invention that man need ever
make, provided that the machine is docile enough to tell us how to keep it under
control.”
They figured that if they could get this recursive self-improvement going, the
machine would soon get smart enough that it could also teach itself all other
human skills that would be useful."

payload <- "content=Lorem%20ipsum%20dolor%20sit%20amet%2C%20consetetur%20sadipscing%20elitr%2C%20sed%20diam%20nonumy%20eirmod%20tempor%20invidunt%20ut%20labore%20et%20dolore%20magna%20aliquyam%20erat%2C%20sed%20diam%20voluptua.%20At%20vero%20eos%20et%20accusam%20et%20justo%20duo%20dolores%20et%20ea%20rebum.%20Stet%20clita%20kasd%20gubergren%2C%20no%20sea%20takimata%20sanctus%20est%20Lorem%20ipsum%20dolor%20sit%20amet.%20Lorem%20ipsum%20dolor%20sit%20amet%2C%20consetetur%20sadipscing%20elitr%2C%20sed%20diam%20nonumy%20eirmod%20tempor%20invidunt%20ut%20labore%20et%20dolore%20magna%20aliquyam%20erat%2C%20sed%20diam%20voluptua.%20At%20vero%20eos%20et%20accusam%20et%20justo%20duo%20dolores%20et%20ea%20rebum.%20Stet%20clita%20kasd%20gubergren%2C%20no%20sea%20takimata%20sanctus%20est%20Lorem%20ipsum%20dolor%20sit%20amet.&response_type=html&request_type=html&fixation=1&saccade=10"

encode <- "form"

response <- VERB("POST",
                 url,
                 body = payload,
                 add_headers('X-RapidAPI-Host' = 'bionic-reading1.p.rapidapi.com',
                             'X-RapidAPI-Key' = 'b34680a5eamsh5c39b8cf066dd0ap1e8d93jsnc9e5097b3c8c'),
                 content_type("application/x-www-form-urlencoded"),
                 encode = encode)

content(response, "text")
