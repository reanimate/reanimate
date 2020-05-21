# Motivation

Narration adds a new dimension to animations. When done well, the narration and animation
will synergize and form a single, coherent experience. When done poorly, the experience will be
disjointed and jarring.

Synchronizing audio and video can be time consuming and difficult to get exactly right even when you
put in a lot of effort. It is typically done in one of three ways:

 1. Manually time and render your entire animation, then record narration over it. This is relatively quick but it is incredibly difficult match audio and visual cues.
 2. Split the animation into small fragments and use video editing software to align the fragments with the narration. This approach usually works well but takes a lot of effort.
 3. Write your animation with timings tied directly to a transcript.

The third approach is by far the least time-consuming and it has built-in support in **reanimate**.
The rest of this article will elaborate on the details.

# Forced aligners

So, how can we tell at that time a word appears in an audio recording? If you've ever seen YouTube's automatic captions then you'll know that voice recognition is far from perfect, often hilariously so. Fortunately, the problem of interpreting an audio recording becomes much simpler if we already have a transcript. Given a transcript, there is a class of programs, called forced aligners, that can tell us exactly when each word is being spoken with nearly 100% accuracy. [Gentle](https://lowerquality.com/gentle/) is one of the most widely used forced aligners and they describe what they do as follows:

> "Forced aligners are computer programs that take media files and their transcripts and return extremely precise timing information for each word (and phoneme) in the media."

# Timings from transcript



<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_transcript.hs!}</code></pre>
</details>
<video width="640" height="360" controls>
  <source src="https://i.imgur.com/9Fxqgkz.mp4">
</video><br/>

# Faking transcripts

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_fake.hs!}</code></pre>
</details>
<video width="640" height="360" controls>
  <source src="https://i.imgur.com/UacO6Qh.mp4">
</video><br/>


# Setting up triggers

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/voice_triggers.hs!}</code></pre>
</details>
<video width="640" height="360" controls>
  <source src="https://i.imgur.com/efPy460.mp4">
</video><br/>

# Indexing words

    A rose is a rose is a rose.

asd

    [beginning]
    Buffalo buffalo Buffalo
    [middle]
    buffalo buffalo buffalo
    [end]
    Buffalo buffalo
