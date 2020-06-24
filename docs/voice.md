# TL;DR

**Reanimate** can automatically synchronize animations to your voice if you have a transcript and an audio recording. This works with the help of [Gentle](https://lowerquality.com/gentle/). Accuracy is not perfect but it is pretty close, and it is by far the easiest way of adding narration to an animation. [API documentation](https://hackage.haskell.org/package/reanimate/docs/Reanimate-Voice.html).

# Motivation

Narration adds a new dimension to animations. When done well, the narration and animation
will synergize and form a single, coherent experience. When done poorly, the experience will be
disjointed and jarring.

Synchronizing audio and video can be time consuming and difficult to get exactly right even when you
put in a lot of effort. It is typically done in one of three ways:

 1. Manually time and render your entire animation, then record narration over it. This is relatively quick but it is incredibly difficult to match audio and visual cues.
 2. Split the animation into small fragments and use video editing software to align the fragments with the narration. This approach usually works well but takes a lot of effort.
 3. Write your animation with timings tied directly to a transcript.

The third approach is by far the least time-consuming and has built-in support in **reanimate**.
The rest of this article goes over the details.

# Forced aligners

So, how can we tell at that time a word appears in an audio recording? If you've ever seen YouTube's automatic captions then you'll know that voice recognition is far from perfect, often hilariously so. Fortunately, the problem of interpreting an audio recording becomes much simpler if we already have a transcript. Given a transcript, there is a class of programs, called forced aligners, that can tell us exactly when each word is being spoken with nearly 100% accuracy. [Gentle](https://lowerquality.com/gentle/) is one of the most widely used forced aligners and they describe what they do as follows:

> "Forced aligners are computer programs that take media files and their transcripts and return extremely precise timing information for each word (and phoneme) in the media."

# Timings from transcript

Transcripts can be read in **reanimate** directly from non-IO code. This works because the transcript is not allowed to change during execution. The core of the API looks like this:

    data Transcript
    data TWord
    transcriptText  :: Transcript -> Text
    transcriptWords :: Transcript -> [TWord]
    
    loadTranscript :: FilePath -> Transcript

Running `loadTranscript "my_transcript.txt"` will first look for a JSON file named `my_transcript.json` containing all the timing information. If no such JSON file could be found, **reanimate** will look for `my_transcript.mp3/m4a/flac` and use Gentle to generate the timing information. The timing information is saved as a JSON file that subsequent calls will use directly.

The video below illustrates how accurate the automatically generated timing information can be:

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_transcript.hs!}</code></pre>
</details>
<video style="width:100%" controls>
  <source src="https://i.imgur.com/9Fxqgkz.mp4">
</video><br/>

# Faking transcripts

Scripts and animations are often developed and revised in parallel. It would be too much work to make audio recordings of drafts but a rough idea of the timing information is paramount when developing the visuals. To this end, **reanimate** can fake timing information and pretend to "read" the script at roughly 120 words per minute.

Faking data is done automatically by `loadTranscript` if no audio file can be found. It's also possible to directly parse a text as a transcript:

    fakeTranscript :: Text -> Transcript

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_fake.hs!}</code></pre>
</details>
<video style="width:100%" controls>
  <source src="https://i.imgur.com/5o0gQ0F.mp4">
</video><br/>

# Setting up triggers

Now that we've covered the basics, let's look at the API for querying timings and setting up triggers in more detail:

    transcriptWords :: Transcript -> [TWord]

    data TWord = TWord
      { wordAligned     :: Text
      , wordCase        :: Text
      , wordStart       :: Double -- ^ Start of pronunciation in seconds
      , wordStartOffset :: Int    -- ^ Character index of word in transcript
      , wordEnd         :: Double -- ^ End of pronunciation in seconds
      , wordEndOffset   :: Int    -- ^ Last character index of word in transcript
      , wordPhones      :: [Phone]
      , wordReference   :: Text   -- ^ The word being pronounced.
      } deriving (Show)

Looking at the `TWord` data structure, the key fields are `wordStart`, `wordEnd`, and `wordReference`. The difference between `wordStart` and `wordEnd` gives the duration of a word, and the video below illustrates how graphical elements can respond to the reading speed:

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_triggers.hs!}</code></pre>
</details>
<video style="width:100%" controls>
  <source src="https://i.imgur.com/efPy460.mp4">
</video><br/>

Words can be looked up in a transcript and ambiguities can be resolved by inserting section markers and using them as keys.

    -- | Locate the first word that occurs after all the given keys.
    --   An error is thrown if no such word exists. An error is thrown
    --   if the keys do not exist in the transcript.
    findWord  :: Transcript -> [Text] -> Text -> TWord
    
    -- | Locate all words that occur after all the given keys.
    --   May return an empty list. An error is thrown
    --   if the keys do not exist in the transcript.
    findWords :: Transcript -> [Text] -> Text -> [TWord]

Below is a transcript with two section markers, 'middle' and 'final'. These markers make it easy to tell the different between the word 'circle' in the first paragraph and the word 'circle' in the second paragraph. In the corresponding video, the words trigger different events depending on which paragraph (or section) they're in. Section markers are only used as references and are not included in the audio recording.

```text
Everything in this animation is timed by my voice.
Every flash, every circle, every square.

[middle]
I decide when everything begins and ends.
Time has run out for the square
and now the circle

[final]
Let's spawn three more circles
and destroy them in a

flash flash flash
```

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_advanced.hs!}</code></pre>
</details>
<video style="width:100%" controls>
  <source src="https://i.imgur.com/5YIaSmB.mp4">
</video><br/>
