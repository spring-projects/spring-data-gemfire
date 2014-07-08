/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import com.gemstone.gemfire.cache.DataPolicy;

/**
 * @author David Turanski
 * @author John Blum
 */
public class DataPolicyConverterTest {

	private final DataPolicyConverter converter = new DataPolicyConverter();

	protected int getDataPolicyEnumerationSize() {
		for (byte ordinal = 0; ordinal < Byte.MAX_VALUE; ordinal++) {
			try {
				assertNotNull(DataPolicy.fromOrdinal(ordinal));
			}
			catch (Exception ignore) {
				return ordinal;
			}
			catch (Error ignore) {
				return ordinal;
			}
		}

		throw new IndexOutOfBoundsException("The size of the Data Policy enumeration could not be determined"
			+ " because the ordinal based on Byte.MAX_VALUE was exhausted!");
	}

	@Test
	public void testPolicyToDataPolicy() {
		// exclude DEFAULT
		assertEquals(getDataPolicyEnumerationSize(), DataPolicyConverter.Policy.values().length - 1);
		assertEquals(DataPolicy.EMPTY, DataPolicyConverter.Policy.EMPTY.toDataPolicy());
		assertEquals(DataPolicy.NORMAL, DataPolicyConverter.Policy.NORMAL.toDataPolicy());
		assertEquals(DataPolicy.PRELOADED, DataPolicyConverter.Policy.PRELOADED.toDataPolicy());
		assertEquals(DataPolicy.PARTITION, DataPolicyConverter.Policy.PARTITION.toDataPolicy());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, DataPolicyConverter.Policy.PERSISTENT_PARTITION.toDataPolicy());
		assertEquals(DataPolicy.REPLICATE, DataPolicyConverter.Policy.REPLICATE.toDataPolicy());
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, DataPolicyConverter.Policy.PERSISTENT_REPLICATE.toDataPolicy());
		assertEquals(DataPolicy.DEFAULT, DataPolicyConverter.Policy.DEFAULT.toDataPolicy());
	}

	@Test
	public void testConvert() {
		assertEquals(DataPolicy.EMPTY, converter.convert("empty"));
		assertEquals(DataPolicy.PARTITION, converter.convert("Partition"));
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, converter.convert("PERSISTENT_REPLICATE"));
		assertNull(converter.convert("invalid"));
		assertNull(converter.convert(null));
	}

}
