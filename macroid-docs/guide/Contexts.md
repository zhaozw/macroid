# Contexts

Android Context is normally required to construct widgets, check screen orientation, etc.
However, one has to [differentiate between](http://stackoverflow.com/questions/987072/using-application-context-everywhere?rq=1)
two types of context: one obtained from an activity
and one obtained from the application. The reason is that holding on to the activity context
may cause [memory leaks](http://stackoverflow.com/questions/3346080/android-references-to-a-context-and-memory-leaks).

*Macroid* automatically stores `Activity` and `Service` contexts as weak references, avoiding the problem.
This is done with the `ContextWrapper` class, which has two methods:

* `contextWrapper.appContext` will always return the application context;
* `contextWrapper.get` will return the original context passed to `ContextWrapper`,
  unless it was not garbage collected, in which case it defaults to `appContext`.

## Including

To include the implicit context in your activity, inherit `Contexts`:

```scala
import macroid.Contexts

class MyActivity extends Activity with Contexts[Activity] {
  ...
}
```

If you use fragments from the support library
(see the [section on fragments](Fragments.html)):

```scala
class MyActivity extends FragmentActivity with Contexts[FragmentActivity] {
  ...
}
```

Finally, in a fragment (either `android.app.Fragment` or `android.support.v4.app.Fragment`):

```scala
class MyFragment extends Fragment with Contexts[Fragment] {
  ...
}
```

You can also construct a `ContextWrapper` directly:

```scala
val ctx = ContextWrapper(myActivity)
```

## Usage

Most *Macroid* APIs require an implicit `ContextWrapper`. If you use them inside an
activity or a fragment, you are golden. However sometimes you need to pass the context
to other methods:

```scala
import macroid.ContextWrapper
import macroid.FullDsl._
import macroid.contrib._

def customTweak(implicit ctx: ContextWrapper) =
  TextTweaks.large +
  text("foo")

def customLayout(implicit ctx: ContextWrapper) =
  l[LinearLayout](
    w[TextView],
    w[Button]
  )
```
