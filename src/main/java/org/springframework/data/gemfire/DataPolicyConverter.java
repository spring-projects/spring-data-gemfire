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

import org.springframework.core.convert.converter.Converter;

import com.gemstone.gemfire.cache.DataPolicy;

/**
 * @author David Turanski
 * 
 */
public class DataPolicyConverter implements Converter<String, DataPolicy> {
	private static enum Policy {
		EMPTY, DEFAULT, NORMAL, PERSISTENT_PARTITION, PERSISTENT_REPLICATE, PRELOADED, REPLICATE;
		public DataPolicy toDataPolicy() {
			DataPolicy dataPolicy = null;
			switch (this) {
			case EMPTY:
				dataPolicy = DataPolicy.EMPTY;
				break;
			case DEFAULT:
				dataPolicy = DataPolicy.DEFAULT;
				break;
			case NORMAL:
				dataPolicy = DataPolicy.NORMAL;
				break;
			case PERSISTENT_PARTITION:
				dataPolicy = DataPolicy.PERSISTENT_PARTITION;
				break;
			case PERSISTENT_REPLICATE:
				dataPolicy = DataPolicy.PERSISTENT_REPLICATE;
				break;
			case PRELOADED:
				dataPolicy = DataPolicy.PRELOADED;
				break;
			case REPLICATE:
				dataPolicy = DataPolicy.REPLICATE;
				break;
			}
			return dataPolicy;
		}

		public static Policy getValue(String value) {
			Policy policy = null;
			try {
				policy = valueOf(value);
			}
			catch (Exception e) {
			}
			return policy;
		}
	};

	@Override
	public DataPolicy convert(String source) {
		if (source == null) {
			return null;
		}
		source = source.toUpperCase();
		return Policy.getValue(source) == null ? null : Policy.getValue(source).toDataPolicy();
	}

}
