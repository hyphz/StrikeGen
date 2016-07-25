
# Strike! RPG Character Generator development notes

## Character Dictionary

The ELM model for the generator is defined in *ModelDb.elm* and has two parts: *character* and *database*. The *character* component is a simple hash from strings to strings which is used to store all parts of the character. This is a bit awkward but makes saving and loading, and form inputs, much easier. (The main reason is so that class and role extensions can be easily written without having to rewrite the JSON encoder for characters to allow for new non-string components. If there's a better way to do this I'd like to know :)

## Forms Model

The whole left-hand side of the user interface is governed by the Forms Model which is defined in *FormsModel.elm* and displayed in *View.elm*.  The ELM architecture *View* function calls *getForms* on the model which is expected to return all the forms (and their contents) that should be displayed. *getForms* is defined in *charModel.elm* with all the basic forms for every character; it also calls *tacticalForms*, from *TacticalModel.elm*, to get the forms specific to tactical combat.  *tacticalForms*, amongst other things, calls the *getForms* members of the selected class and role to add their forms to the returned list.

Each form has a name and a list of fields. There are three types of fields: dropdown, freeform, and number (which is only used for level). Every field has a *name*, which is what is displayed in the form, and a *key*, which is the string key used for that entry in the character dictionary. Entering "debugfieldkeys" (without the quotes) as the name of your character will change the forms to display the key instead of the name which can be useful for debugging, especially if keys are being automatically generated.

There's no validation built into the forms model because most freeform inputs don't need to be validated; they're just text to be copied onto the character sheet. When a field value is changed the view sends a *FormFieldUpdated* message which is picked up in *CharModel*; *CharModel.updateFieldResponse* copies the response into the character dictionary, and *CharModel.fieldChanged* is then called to do validation on the few fields that need it.

## Loading Sequence

When the generater is first loaded a series of messages are passed back between JS and Elm as follows, to load the initial data files:

* The ELM architecture initializer *CharModel.init* uses getJsonFileCommand and the standard HTTP loader to load backgrounds.json and return it with the message *BackgroundsLoaded*.
* *BackgroundsLoaded* is picked up by *ModelDB.dbUpdate* which decodes it with *ModelDB.unpackBackgrounds* follows with another HTTP load request returning *OriginsLoaded*.
* *OriginsLoaded* is also picked up by *ModelDB.update* which decodes it with *ModelDB.unpackOrigins* and sends a HTTP load request returning *KitsLoaded*.
* *KitsLoaded* is picked up, the result decoded with *unpackKits*, and a new HTTP request sent for the text database with message *TextsLoaded*.
* *TextLoaded* is picked up and the result decoded with *TextsLoaded*. (For some unknown reason loading the text database at any point other than last results in a security error in the HTTP loader.) Then the port message *Ports.dbLoaded* is sent to the JavaScript to let it know loading is complete.
* *dbLoaded* is caught inside *StrikeGen.html*, which checks if there was a *?load=* parameter in the URL. If there wasn't, it stops and no further messages are passed. If there was, then to load this character, it decompresses the contents of the *load* parameter using LZString and then sends a *loadJson* message to the ELM.
* *loadJson* is picked up in *CharModel.update* which passes it to *importChar*, which decodes the JSON and loads all its contents into the character dictionary.

## Basic character data

Backgrounds are loaded from *data\backgrounds.json* and have the following components:

* *name*: the name for selection. The selected background is stored in dictionary entry **basics-bg**.
* *skillNames*: the list of all skills provided.
* *wealth*: the base wealth level as a number.
* *trick*: the trick provided.

There's nothing too fancy about backgrounds: you more or less just pick one and it's installed. The exception is the "custom" background. If this is selected, *CharModel.customBackgroundForm* is added to the form list, loading the custom skills into the hash:

* First four skills **bg-custom-1**..**4**.
* Trick, **bg-custom-t**.
* First extra choice of skill or wealth. **bg-custom-wos1** is "People" or "Wealth"; if it's "People" the skill name is in **bg-custom-wos1s**.
* Ditto for the second: **bg-custom-wos2** is "Skill" or "Money"; if it's "Skill" the skill name is in **bg-custom-wos2s**.

Origins are a little more complex since they have more variability. They are loaded from *data\origins.json* and contain:

* *name*: the name for selection. The selected origin is stored in **basics-origin**.
* *skillNames*: the list of skills possible. The rules state that an Origin only ever gives at most 2 skills so if there are more than 2 skills listed here, the program treats this as a "complex origin" (detected by *CharModel.hasComplexOrigin*) and offers choices for the skills. The form used is *CharModel.complexOriginForm* and the two skill choices are placed in the character dictionary at **origin-s1** and **origin-s2**.
* *wealth*: the wealth modifier.
* *complications*: The list of possible complications from which the player picks one. As before, having more than one here makes this a complex origin and the selected complication is stored at **origin-co**.
* *freeformSkill*: optional. If present and true then a box is added to the origin form allowing the user to enter a freeform skill which is stored at **origin-cs**.
* *freeformComplication*: like freeformSkill, but the complication is stored at **origin-cco**.

If a custom origin is selected, then *CharModel.customOriginForm* is used instead of the complex origin form, and loads the following:

* The origin's skill in **origin-custom-s1**.
* A choice of a second skill or wealth in **origin-custom-wos1**. If it's "Skill" then the skill name in **origin-custom-s2**.
* The complication in **origin-custom-co**.

Fortunately, the functions *CharModel.resolvedOriginSkills* and *CharModel.resolvedOriginComplications* deal with fetching the final lists of skills and complications that are generated by the origin, allowing for all of these fiddly bits.
