/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;

import org.junit.Test;

import com.gemstone.gemfire.internal.GemFireVersion;

/**
 * The GemfireUtilsTest class is a test suite of test cases testing the contract and functionality of the GemfireUtils
 * abstract utility class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.GemfireUtils
 * @since 1.3.3
 */
public class GemfireUtilsTest {

	// NOTE implementation is based on a GemFire internal class... com.gemstone.gemfire.internal.GemFireVersion.
	protected int getGemFireVersion() {
		try {
			String gemfireVersion = GemFireVersion.getGemFireVersion();
			StringBuilder buffer = new StringBuilder();

			buffer.append(GemFireVersion.getMajorVersion(gemfireVersion));
			buffer.append(GemFireVersion.getMinorVersion(gemfireVersion));

			return Integer.decode(buffer.toString());
		}
		catch (NumberFormatException ignore) {
			return -1;
		}
	}

	@Test
	public void testIsGemfireVersion65OrAbove() {
		int gemfireVersion = getGemFireVersion();
		assumeTrue(gemfireVersion > -1);
		assertEquals(getGemFireVersion() >= 65, GemfireUtils.isGemfireVersion65OrAbove());
	}

	@Test
	public void testIsGemfireVersion7OrAbove() {
		int gemfireVersion = getGemFireVersion();
		assumeTrue(gemfireVersion > -1);
		assertEquals(getGemFireVersion() >= 70, GemfireUtils.isGemfireVersion7OrAbove());
	}

}
