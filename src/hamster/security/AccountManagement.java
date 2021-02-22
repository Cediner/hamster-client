package hamster.security;

import hamster.io.Storage;
import haven.MessageBuf;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

/**
 * Stored in dynamic.sqlite
 * Table: account_blob
 * | Name (PK) | data (Encrypted blob) |
 */
public class AccountManagement {
    public static class Account {
        public String username;
        public String password;

        public Account(final String uname, final String pass) {
            this.username = uname;
            this.password = pass;
        }

        public Account() {
            this.username = "";
            this.password = "";
        }
    }

    private final String name;
    private final List<Account> accounts;
    private final byte[] key;

    public AccountManagement(final byte[] key, final String name) throws Exception {
        this.name = name;
        DerivedKeyGen keyg = new DerivedKeyGen();
        this.key = keyg.generateKey(key, "slow-and-steady".getBytes());
        accounts = new ArrayList<>();
        load(name);
    }

    public Account addAccount() {
        final Account acc = new Account();
        accounts.add(acc);
        return acc;
    }

    public void remAccount(final Account acc) {
        accounts.remove(acc);
    }

    public Account get(int ind) {
        return accounts.get(ind);
    }

    public int size() {
        return accounts.size();
    }

    public void save() throws Exception {
        final MessageBuf buf = new MessageBuf();
        for (final Account acc : accounts) {
            buf.addstring(acc.username);
            buf.addstring(acc.password);
        }

        final byte[] data = Crypto.encrypt(buf.fin(), key);

        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO account_blob VALUES (?, ?)");
            stmt.setString(1, name);
            stmt.setBytes(2, data);
            stmt.executeUpdate();
        });
    }

    /*   +------------------------+
     *   | # of Accounts  - int   +
     *   +------------------------+
     *   | Account Name - String  |
     *   | Password - String      |
     *   |------------------------|
     *   | Repeat till eof        |
     *   +------------------------|
     */
    private void load(final String name) throws Exception {
        final PreparedStatement stmt = Storage.dynamic.prepare("SELECT data FROM account_blob WHERE name = ?");
        stmt.setString(1, name);
        try (final ResultSet res = stmt.executeQuery()) {
            if (res.next()) {
                //Existing account with data, otherwise new account with nothing
                final byte[] blob = res.getBytes(1);
                final MessageBuf data = new MessageBuf(Crypto.decrypt(blob, key));
                while (!data.eom()) {
                    accounts.add(new Account(data.string(), data.string()));
                }
            }
        }
    }
}
