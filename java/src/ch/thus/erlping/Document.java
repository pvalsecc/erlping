package ch.thus.erlping;

import com.orientechnologies.orient.client.remote.OServerAdmin;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.record.ORecord;
import com.orientechnologies.orient.core.record.impl.ODocument;

import java.io.IOException;

public class Document {

    public static final String DB_URL = "remote:localhost/test";

    public static void main(String[] args) throws IOException {
        OServerAdmin admin = new OServerAdmin(DB_URL)
                .connect("root", "root")
                .createDatabase("graph", "plocal");
        try {
            ODatabaseDocumentTx db = new ODatabaseDocumentTx(DB_URL).open("root", "root");
            try {
                ORecord record = db.load(new ORecordId(0, 1), "");

                db.begin();
                ODocument doc = new ODocument("V");
                doc.field("toto", 12);
                doc.field("tutu", "tutu");

                doc.save(true);
                db.commit();
            } finally {
                db.close();
            }
        } finally {
            admin.dropDatabase("test");
            admin.close();
        }
    }
}
