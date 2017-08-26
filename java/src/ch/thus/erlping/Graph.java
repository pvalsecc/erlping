package ch.thus.erlping;

import com.orientechnologies.orient.client.remote.OServerAdmin;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.orient.OrientGraph;

import static ch.thus.erlping.Document.DB_URL;

public class Graph {
    public static void main(String[] args) throws Exception {
        OServerAdmin admin = new OServerAdmin(DB_URL)
                .connect("root", "root")
                .createDatabase("graph", "plocal");
        try {
            OrientGraph graph = new OrientGraph(DB_URL);
            try {
                try {
                    Vertex v1 = graph.addVertex(null);
                    v1.setProperty("toto", 12);
                    v1.setProperty("tutu", "tutu");

                    Vertex v2 = graph.addVertex(null);
                    v2.setProperty("x", 4.2);

                    graph.addEdge(null, v1, v2, "E");
                    graph.commit();
                } catch (Exception e) {
                    graph.rollback();
                    throw e;
                }
            } finally {
                graph.shutdown();
            }
        } finally {
            //admin.dropDatabase("test");
            admin.close();
        }
    }
}

/*
Send: O_TX_COMMIT
0000   3c 00 00 00 21 00 00 00 6f 00 00 00 00 04 74 65  <...!...o.....te
0010   73 74 00 00 05 00 00 00 00 00 00 00 00 00 00 01  st..............
0020   5d b8 ba 7d fe 00 00 24 00 17 4f 52 65 63 6f 72  ]..}...$..ORecor
0030   64 53 65 72 69 61 6c 69 7a 65 72 42 69 6e 61 72  dSerializerBinar
0040   79 00 0d 4f 72 69 65 6e 74 44 42 20 4a 61 76 61  y..OrientDB Java
0050   00 06 32 2e 32 2e 32 34 c9 6a 75 9b 1f 23 8f e7  ..2.2.24.ju..#..
0060   ed 88 2a bd 05 21 6f 06 f5 69 2f f2 2f 78 3e 1f  ..*..!o..i/./x>.
0070   47 b2 71 b4 5c 9b c8 d1 00 00 00 01 01 01 03 ff  G.q.\...........
0080   ff ff ff ff ff ff ff ff fe 64 00 00 00 47 00 02  .........d...G..
0090   56 08 74 6f 74 6f 00 00 00 22 01 08 74 75 74 75  V.toto..."..tutu
00a0   00 00 00 23 07 08 6f 75 74 5f 00 00 00 28 16 00  ...#..out_...(..
00b0   18 08 74 75 74 75 03 95 33 e8 cb 47 76 48 ab 97  ..tutu..3..GvH..
00c0   6d c8 c3 81 51 66 7d 00 00 00 01 ff ff ff ff ff  m...Qf}.........
00d0   ff ff ff ff fc 01 03 ff ff ff ff ff ff ff ff ff  ................
00e0   fd 64 00 00 00 3b 00 02 56 02 78 00 00 00 14 05  .d...;..V.x.....
00f0   06 69 6e 5f 00 00 00 1c 16 00 40 10 cc cc cc cc  .in_......@.....
0100   cc cd 03 75 73 5f 80 a5 dc 4d cb ac fd 28 05 11  ...us_...M...(..
0110   aa a5 4e 00 00 00 01 ff ff ff ff ff ff ff ff ff  ..N.............
0120   fc 01 03 ff ff ff ff ff ff ff ff ff fc 64 00 00  .............d..
0130   00 19 00 02 45 06 6f 75 74 00 00 00 15 0d 04 69  ....E.out......i
0140   6e 00 00 00 17 0d 00 01 03 01 05 00 00 00 00 03  n...............
0150   00 00 00                                         ...

Recv:
0000   00 00 00 00 21 00 00 00 00 00 00 00 03 ff ff ff  ....!...........
0010   ff ff ff ff ff ff fd 00 0a 00 00 00 00 00 00 00  ................
0020   00 ff ff ff ff ff ff ff ff ff fc 00 11 00 00 00  ................
0030   00 00 00 00 00 ff ff ff ff ff ff ff ff ff fe 00  ................
0040   09 00 00 00 00 00 00 00 00 00 00 00 03 00 0a 00  ................
0050   00 00 00 00 00 00 00 00 00 00 01 00 09 00 00 00  ................
0060   00 00 00 00 00 00 00 00 01 00 11 00 00 00 00 00  ................
0070   00 00 00 00 00 00 01 00 00 00 02 95 33 e8 cb 47  ............3..G
0080   76 48 ab 97 6d c8 c3 81 51 66 7d ff ff ff ff ff  vH..m...Qf}.....
0090   ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff 75  ...............u
00a0   73 5f 80 a5 dc 4d cb ac fd 28 05 11 aa a5 4e ff  s_...M...(....N.
00b0   ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff  ................
00c0   ff ff ff                                         ...

0000   00 00 00 00 20 00 00 00 00                       .... ....
 */
