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

package org.springframework.data.gemfire.test;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.test.annotation.ProfileValueSource;

/**
 * The GemfireProfileValueSource class is a custom Spring test framework ProfileValueSource used to determine
 * profile and environment specific configuration for test enablement.
 *
 * @author John Blum
 * @see org.springframework.test.annotation.ProfileValueSource
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class GemfireProfileValueSource implements ProfileValueSource {

	public static final String APACHE_GEODE_PRODUCT_NAME = "Apache Geode";
	public static final String PIVOTAL_GEMFIRE_PRODUCT_NAME = "Pivotal GemFire";
	public static final String PRODUCT_NAME_KEY = "product.name";

	public static final Map<String, String> PROFILE_VALUES;

	static {
		Map<String, String> profileValues = new ConcurrentHashMap<String, String>(1);
		profileValues.put(PRODUCT_NAME_KEY, System.getProperty(PRODUCT_NAME_KEY, GemfireUtils.GEMFIRE_NAME));
		PROFILE_VALUES = Collections.unmodifiableMap(profileValues);
	}

	public static boolean isApacheGeode() {
		return APACHE_GEODE_PRODUCT_NAME.equals(getProfileValue(PRODUCT_NAME_KEY));
	}

	public static boolean isPivotalGemFire() {
		return PIVOTAL_GEMFIRE_PRODUCT_NAME.equals(getProfileValue(PRODUCT_NAME_KEY));
	}

	public static String getProfileValue(final String profileKey) {
		return PROFILE_VALUES.get(profileKey);
	}

	@Override
	public String get(final String key) {
		return getProfileValue(key);
	}

}
