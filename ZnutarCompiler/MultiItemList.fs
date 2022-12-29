namespace Znutar

/// A list that contains at least two items.
type MultiItemList<'t> =
    {
        Item0 : 't
        Item1 : 't
        Rest : List<'t>
    }
    with

    member private this.Items =
        seq {
            yield this.Item0
            yield this.Item1
            yield! this.Rest
        }

    interface System.Collections.Generic.IEnumerable<'t> with

        member this.GetEnumerator() = 
            this.Items.GetEnumerator()
                :> System.Collections.IEnumerator

        member this.GetEnumerator() =
            this.Items.GetEnumerator()

module MultiItemList =

    let create item0 item1 rest =
        {
            Item0 = item0
            Item1 = item1
            Rest = rest
        }

    let map mapping list =
        {
            Item0 = mapping list.Item0
            Item1 = mapping list.Item1
            Rest = List.map mapping list.Rest
        }
