package hamster.security;

import org.bouncycastle.crypto.digests.SHA256Digest;
import org.bouncycastle.crypto.macs.HMac;
import org.bouncycastle.crypto.prng.SP800SecureRandom;
import org.bouncycastle.crypto.prng.SP800SecureRandomBuilder;

//Used to generate random bytes of various lengths based on keys as seed
class RndKeyGen {
    private final SP800SecureRandom rnd;

    RndKeyGen() {
        SP800SecureRandomBuilder rndbuild = new SP800SecureRandomBuilder();
        rnd = rndbuild.buildHMAC(new HMac(new SHA256Digest()), null, false);
    }

    byte[] genBytes(int bytes) {
        byte[] ret = new byte[bytes];
        rnd.nextBytes(ret);
        return ret;
    }
}
