namespace Znutar

/// A list that contains at least two items.
type MultiItemList<'t> =
    {
        Item1 : 't
        Item2 : 't
        Rest : List<'t>
    }

    member this.Length =
        this.Rest.Length + 2

    member this.Items =
        seq {
            yield this.Item1
            yield this.Item2
            yield! this.Rest
        }

    interface System.Collections.Generic.IEnumerable<'t> with

        member this.GetEnumerator() = 
            this.Items.GetEnumerator()
                :> System.Collections.IEnumerator

        member this.GetEnumerator() =
            this.Items.GetEnumerator()

module MultiItemList =

    let create item1 item2 rest =
        {
            Item1 = item1
            Item2 = item2
            Rest = rest
        }

    let map mapping milist =
        {
            Item1 = mapping milist.Item1
            Item2 = mapping milist.Item2
            Rest = List.map mapping milist.Rest
        }
