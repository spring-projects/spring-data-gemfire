/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.client;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.springframework.data.gemfire.client.Interest.Type.KEY;
import static org.springframework.data.gemfire.client.Interest.Type.REGEX;
import static org.springframework.data.gemfire.client.Interest.newInterest;

import java.util.List;

import org.apache.geode.cache.InterestResultPolicy;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link Interest}.
 *
 * @author John Blum
 * @author Mark Paluch
 * @see org.junit.Test
 * @see org.apache.geode.cache.InterestResultPolicy
 * @see org.springframework.data.gemfire.client.Interest
 * @since 1.6.0
 */
public class InterestUnitTests {

	protected static final boolean DURABLE = true;
	protected static final boolean DO_NOT_RECEIVE_VALUES = false;

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void constructInterestWithKey() {
		Interest<String> interest = new Interest<>("testKey");

		assertThat(interest.getKey()).isEqualTo("testKey");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.KEYS_VALUES);
		assertThat(interest.isDurable()).isFalse();
		assertThat(interest.isReceiveValues()).isTrue();
		assertThat(interest.getType()).isEqualTo(KEY);

		String expectedString = String.format(
			"{ @type = %s, key = testKey, durable = false, policy = KEYS_VALUES, receiveValues = true, type = KEY }",
				interest.getClass().getName());

		assertThat(interest.toString()).isEqualTo(expectedString);
	}

	@Test
	public void constructInterestWithKeyAndPolicy() {
		Interest<String> interest = new Interest<>("mockKey", InterestResultPolicy.KEYS);

		assertThat(interest.getKey()).isEqualTo("mockKey");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.KEYS);
		assertThat(interest.isDurable()).isFalse();
		assertThat(interest.isReceiveValues()).isTrue();
		assertThat(interest.getType()).isEqualTo(KEY);

		String expectedString = String.format(
			"{ @type = %s, key = mockKey, durable = false, policy = KEYS, receiveValues = true, type = KEY }",
				interest.getClass().getName());

		assertThat(interest.toString()).isEqualTo(expectedString);
	}

	@Test
	public void constructInterestWithKeyPolicyAndDurability() {
		Interest<String> interest = new Interest<>(".*Key", InterestResultPolicy.NONE, DURABLE);

		assertThat(interest.getKey()).isEqualTo(".*Key");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.NONE);
		assertThat(interest.isDurable()).isTrue();
		assertThat(interest.isReceiveValues()).isTrue();
		assertThat(interest.getType()).isEqualTo(REGEX);

		String expectedString = String.format(
			"{ @type = %s, key = .*Key, durable = true, policy = NONE, receiveValues = true, type = REGEX }",
				interest.getClass().getName());

		assertThat(interest.toString()).isEqualTo(expectedString);
	}

	@Test
	public void constructInterestWithKeyPolicyDurabilityAndReceiveValues() {
		List<String> keys = asList("KeyOne", "KeyTwo", "KeyThree");

		Interest<Object> interest = new Interest<>(keys, InterestResultPolicy.KEYS_VALUES,
			DURABLE, DO_NOT_RECEIVE_VALUES);

		assertThat(interest.getKey()).isEqualTo(keys);
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.KEYS_VALUES);
		assertThat(interest.isDurable()).isTrue();
		assertThat(interest.isReceiveValues()).isFalse();
		assertThat(interest.getType()).isEqualTo(Interest.Type.KEY);

		String expectedString = String.format(
			"{ @type = %s, key = [KeyOne, KeyTwo, KeyThree], durable = true, policy = KEYS_VALUES, receiveValues = false, type = KEY }",
				interest.getClass().getName());

		assertThat(interest.toString()).isEqualTo(expectedString);
	}

	@Test
	public void constructInterestWithNullKey() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Key is required");

		new Interest<>(null);
	}

	@Test
	@SuppressWarnings("deprecation")
	public void constructInterestWithStringPolicy() {
		Interest<String> interest = new Interest<>("mockKey", "nOnE");

		assertThat(interest.getKey()).isEqualTo("mockKey");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.NONE);
		assertThat(interest.getType()).isEqualTo(KEY);
	}

	@Test(expected = IllegalArgumentException.class)
	@SuppressWarnings("deprecation")
	public void constructInterestWithInvalidStringPolicy() {
		new Interest<>("testKey", "INVALID");
	}

	@Test
	public void isAlphanumericWhitespace() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.isAlphaNumericWhitespace('a')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('X')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('0')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('1')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('2')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('4')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('8')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace('9')).isTrue();
		assertThat(interest.isAlphaNumericWhitespace(' ')).isTrue();
	}

	@Test
	public void isNonAlphanumericWhitespace() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.isNotAlphaNumericWhitespace('@')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('$')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('.')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('_')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('-')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('+')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('*')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('?')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('\\')).isTrue();
		assertThat(interest.isNotAlphaNumericWhitespace('[')).isTrue();
	}

	@Test
	public void containsNonAlphanumericWhitespace() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.containsNonAlphaNumericWhitespace(".*")).isTrue();
		assertThat(interest.containsNonAlphaNumericWhitespace(".*Key")).isTrue();
		assertThat(interest.containsNonAlphaNumericWhitespace("\\d")).isTrue();
		assertThat(interest.containsNonAlphaNumericWhitespace("\\s")).isTrue();
		assertThat(interest.containsNonAlphaNumericWhitespace("p\\{Alnum}")).isTrue();
		assertThat(interest.containsNonAlphaNumericWhitespace("p\\{Space}")).isTrue();
	}

	@Test
	public void containsOnlyAlphanumericWhitespace() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.containsNonAlphaNumericWhitespace("0")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("123")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("123 456")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("key")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("keyOne")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("Key One")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("key0")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("key1")).isFalse();
		assertThat(interest.containsNonAlphaNumericWhitespace("key123")).isFalse();
	}

	@Test
	public void isRegularExpression() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.isRegularExpression(".?")).isTrue();
		assertThat(interest.isRegularExpression(".+")).isTrue();
		assertThat(interest.isRegularExpression(".*")).isTrue();
		assertThat(interest.isRegularExpression(".*Key")).isTrue();
		assertThat(interest.isRegularExpression("\\d")).isTrue();
		assertThat(interest.isRegularExpression("\\n")).isTrue();
		assertThat(interest.isRegularExpression("\\s")).isTrue();
		assertThat(interest.isRegularExpression("p\\{Alnum}")).isTrue();
		assertThat(interest.isRegularExpression("p\\{Space}")).isTrue();
		assertThat(interest.isRegularExpression("^abc$")).isTrue();
		assertThat(interest.isRegularExpression(" [abc]  ")).isTrue();
		assertThat(interest.isRegularExpression("a{0,}bc")).isTrue();
		assertThat(interest.isRegularExpression("a{1,10} bc*")).isTrue();
	}

	@Test
	public void isNotRegularExpression() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.isRegularExpression("abc")).isFalse();
		assertThat(interest.isRegularExpression("123")).isFalse();
		assertThat(interest.isRegularExpression("abc123")).isFalse();
		assertThat(interest.isRegularExpression("   abc 123  ")).isFalse();
		assertThat(interest.isRegularExpression("lOlO")).isFalse();
		assertThat(interest.isRegularExpression((Object) "ALL_KEYS")).isFalse();
		assertThat(interest.isRegularExpression(asList("a", "b", "c"))).isFalse();
	}

	@Test
	public void resolveTypeIsCorrect() {
		assertThat(newInterest(".*").resolveType(KEY)).isEqualTo(KEY);
		assertThat(newInterest("key").resolveType(REGEX)).isEqualTo(REGEX);
		assertThat(newInterest("key").resolveType(null)).isEqualTo(KEY);
		assertThat(newInterest(asList("a", "b", "c")).resolveType(null)).isEqualTo(KEY);
		assertThat(newInterest(".*").resolveType(null)).isEqualTo(REGEX);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void setAndGetStateIsCorrect() {
		Interest<String> interest = newInterest("key");

		assertThat(interest.isDurable()).isFalse();
		assertThat(interest.getKey()).isEqualTo("key");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.DEFAULT);
		assertThat(interest.isReceiveValues()).isTrue();
		assertThat(interest.getType()).isEqualTo(KEY);

		interest.setDurable(true);
		interest.setKey("testKey");
		interest.setPolicy(InterestResultPolicy.KEYS);
		interest.setReceiveValues(false);
		interest.setType(Interest.Type.REGEX);

		assertThat(interest.isDurable()).isTrue();
		assertThat(interest.getKey()).isEqualTo("testKey");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.KEYS);
		assertThat(interest.isReceiveValues()).isFalse();
		assertThat(interest.getType()).isEqualTo(REGEX);
	}

	@Test
	public void setAndGetPolicy() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.DEFAULT);

		interest.setPolicy(InterestResultPolicy.NONE);

		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.NONE);

		interest.setPolicy("keys");

		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.KEYS);
	}

	@Test(expected = IllegalArgumentException.class)
	public void setPolicyWithIllegalValueThrowsException() {
		newInterest("key").setPolicy("ILLEGAL");
	}

	@Test
	public void isKeyTypeIsCorrect() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.getType()).isEqualTo(KEY);
		assertThat(interest.isKeyType()).isTrue();

		interest.setType(REGEX);

		assertThat(interest.getType()).isEqualTo(REGEX);
		assertThat(interest.isKeyType()).isFalse();

		interest.setType(KEY);

		assertThat(interest.getType()).isEqualTo(KEY);
		assertThat(interest.isKeyType()).isTrue();
	}

	@Test
	public void isRegexTypeIsCorrect() {
		Interest<?> interest = newInterest("key");

		assertThat(interest.getType()).isEqualTo(KEY);
		assertThat(interest.isRegexType()).isFalse();

		interest.setType(KEY);

		assertThat(interest.getType()).isEqualTo(KEY);
		assertThat(interest.isRegexType()).isFalse();

		interest.setType(REGEX);

		assertThat(interest.getType()).isEqualTo(REGEX);
		assertThat(interest.isRegexType()).isTrue();
	}

	@Test
	public void newInterestWithBuilder() {
		Interest<?> interest = newInterest(".*").makeDurable().receivesValues(false)
			.usingPolicy(InterestResultPolicy.KEYS);

		assertThat(interest).isNotNull();
		assertThat(interest.isDurable()).isTrue();
		assertThat(interest.getKey()).isEqualTo(".*");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.KEYS);
		assertThat(interest.isReceiveValues()).isFalse();
		assertThat(interest.getType()).isEqualTo(REGEX);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void newInterestWithBuilderHonorsType() {
		Interest<?> interest = newInterest(".*").asType(KEY).withKey("^.+Key\\p{Digit}$")
			.usingPolicy(InterestResultPolicy.NONE);

		assertThat(interest).isNotNull();
		assertThat(interest.isDurable()).isFalse();
		assertThat(interest.getKey()).isEqualTo("^.+Key\\p{Digit}$");
		assertThat(interest.getPolicy()).isEqualTo(InterestResultPolicy.NONE);
		assertThat(interest.isReceiveValues()).isTrue();
		assertThat(interest.getType()).isEqualTo(KEY);
	}
}
