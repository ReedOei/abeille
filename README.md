# Overview

abeille is a tool for creating JSON schema objects/parsers (in Prolog) for JSON.

## Example

Run the following command (in the repository):

```
$ cat examples/basic.json | ./abeille.pl
top_result(top(Result,People),Result).
top_people(top(Result,People),People).
result_succeeded(result(Succeeded,Code),Succeeded).
result_code(result(Succeeded,Code),Code).
people_name(people(Name,Age,Height),Name).
people_age(people(Name,Age,Height),Age).
people_height(people(Name,Age,Height),Height).

parse_top(json(Json),top(Result,People)):-member(result=TempResult,Json),parse_result(TempResult,Result),member(people=TempPeople,Json),maplist(parse_people,TempPeople,People).
parse_result(json(Json),result(Succeeded,Code)):-member(succeeded=Succeeded,Json),member(code=Code,Json).
parse_people(json(Json),people(Name,Age,Height)):-member(name=Name,Json),member(age=Age,Json),member(height=Height,Json).
```

The input JSON file:

```json
{
    "result": {
        "succeeded": true,
        "code": 200
    },

    "people": [
        {
            "name": "Reed",
            "age": 20,
            "height": 71
        },
        {
            "name": "Joe",
            "age": 31,
            "height": 73
        }
    ],
}
```

## Random

The name `abeille` is the French word for "bee", chosen because "apis" (the plural of API) is Latin for "bee", but "apis" isn't as good a repository name.

