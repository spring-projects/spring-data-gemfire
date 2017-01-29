/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.DataPolicy;
import org.springframework.core.convert.converter.Converter;

/**
 * The DataPolicyConverter class converts String values into GemFire DataPolicy enumerated values.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.core.convert.converter.Converter
 * @see org.apache.geode.cache.DataPolicy
 */
public class DataPolicyConverter implements Converter<String, DataPolicy> {

	static enum Policy {
		DEFAULT, EMPTY, NORMAL, PRELOADED, PARTITION, PERSISTENT_PARTITION, REPLICATE, PERSISTENT_REPLICATE;

		private static String toUpperCase(String value) {
			return (value == null ? null : value.toUpperCase());
		}

		public static Policy getValue(String value) {
			try {
				return valueOf(toUpperCase(value));
			}
			catch (Exception e) {
				return null;
			}
		}

		public DataPolicy toDataPolicy() {
			switch (this) {
				case EMPTY:
					return DataPolicy.EMPTY;
				case NORMAL:
					return DataPolicy.NORMAL;
				case PRELOADED:
					return DataPolicy.PRELOADED;
				case PARTITION :
					return DataPolicy.PARTITION;
				case PERSISTENT_PARTITION:
					return DataPolicy.PERSISTENT_PARTITION;
				case REPLICATE:
					return DataPolicy.REPLICATE;
				case PERSISTENT_REPLICATE:
					return DataPolicy.PERSISTENT_REPLICATE;
				case DEFAULT:
				default:
					return DataPolicy.DEFAULT;
			}
		}
	}

	@Override
	public DataPolicy convert(String policyValue) {
		Policy policy = Policy.getValue(policyValue);
		return (policy == null ? null : policy.toDataPolicy());
	}

}
