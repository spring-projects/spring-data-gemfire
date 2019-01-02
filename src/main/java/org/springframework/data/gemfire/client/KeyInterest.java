/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client;

import org.apache.geode.cache.InterestResultPolicy;

/**
 * Cache Region interest based on individual keys.
 *
 * @author John Blum
 * @param <K> {@link Class} type of the key.
 * @see org.springframework.data.gemfire.client.Interest
 */
@SuppressWarnings("unused")
public class KeyInterest<K> extends Interest<K> {

	public KeyInterest(K key) {
		super(key);
	}

	public KeyInterest(K key, InterestResultPolicy policy) {
		super(key, policy);
	}

	public KeyInterest(K key, InterestResultPolicy policy, boolean durable) {
		super(key, policy, durable);
	}

	public KeyInterest(K key, InterestResultPolicy policy, boolean durable, boolean receiveValues) {
		super(key, policy, durable, receiveValues);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Type getType() {
		return Type.KEY;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setType(Type type) {
		logger.warn(String.format("Setting the Type [%1$s] of Interest on [%2$s] is ignored",
			type, getClass().getName()));
	}
}
