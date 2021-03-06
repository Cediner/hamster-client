package hamster.security;

import org.bouncycastle.crypto.digests.SHA256Digest;
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator;
import org.bouncycastle.crypto.params.KeyParameter;

class DerivedKeyGen {
    private static final int DEFAULT_ITER = 64000;

    private final PKCS5S2ParametersGenerator gen;

    DerivedKeyGen() {
        gen = new PKCS5S2ParametersGenerator(new SHA256Digest());
    }

    /* Generates a new derived key from our given key
     */
    byte[] generateKey(final byte[] key, final byte[] salt) {
        gen.init(key, salt, DEFAULT_ITER);
        return ((KeyParameter) gen.generateDerivedParameters(256)).getKey();
    }
}
