/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.client;

import org.apache.geode.cache.InterestResultPolicy;
import org.springframework.util.Assert;

/**
 * Cache interest based on regular expression rather then individual key types.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.data.gemfire.client.Interest
 */
@SuppressWarnings("unused")
public class RegexInterest extends Interest<String> {

	public RegexInterest(String regex) {
		super(regex);
	}

	public RegexInterest(String regex, InterestResultPolicy policy) {
		super(regex, policy);
	}

	public RegexInterest(String regex, InterestResultPolicy policy, boolean durable) {
		super(regex, policy, durable);
	}

	public RegexInterest(String regex, InterestResultPolicy policy, boolean durable, boolean receiveValues) {
		super(regex, policy, durable, receiveValues);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() {
		Assert.hasText(getKey(), "Regex is required");
	}

	/**
	 * Returns the Regular Expression sent to the cache server to express interests in keys matching Regex pattern.
	 *
	 * Alias for {@link #getKey()}.
	 *
	 * @return the Regex pattern used in the interest registration.
	 * @see org.apache.geode.cache.Region#registerInterestRegex(String)
	 */
	public String getRegex() {
		return getKey();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Type getType() {
		return Type.REGEX;
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
