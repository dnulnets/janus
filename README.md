# Janus

![Janus login page](/documentation/login.png)

An Application/Product Lifecycle Management solution handling products with regards to user needs, requirements, issues, teams, backlogs and releases for development, building and maintaining products and services, focusing on their entire life cycle.

**_NOTE:_** From **wikipedia**: In ancient Roman religion and myth, Janus (/ˈdʒeɪnəs/ JAY-nəs; Latin: Ianus [ˈi̯aːnʊs]) is the god of beginnings, gates, transitions, time, duality, doorways, passages, frames, and endings.

It will contain support for teams, product owners and single individuals tasks to maintain the product or service.

## Status
The project is in an early design and startup phase. It started 2023-01-10 and I am currently creating the code framework for the solution so I can start build on functionality. It uses a lot of structuring, functions and ideas from Thomas Honeymans [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld), which by the way is a great place to start for building applications using halogen.

The frontend application is built with purescript using halogen. The backend api and static file server is built with haskell using scotty.

### News

The core structure of the application UI is in place together with i18n-support, login and administrative functions for users and roles. The corresponding backend REST-api in haskell and postgresql is also in place. So you can start the application, login and administer users. I know, not very impressive so far, but is is needed to move on.
