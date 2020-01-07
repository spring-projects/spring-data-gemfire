/*
 * Copyright 2016-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.support.GemfireBeanFactoryLocator.BeanFactoryReference.UNINITIALIZED_BEAN_FACTORY_REFERENCE_MESSAGE;
import static org.springframework.data.gemfire.support.GemfireBeanFactoryLocator.newBeanFactoryLocator;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.beans.factory.BeanFactory;

/**
 * Unit tests for {@link GemfireBeanFactoryLocator}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireBeanFactoryLocatorUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private BeanFactory mockBeanFactory;

	@Before
	public void setup() {
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.isEmpty()).isTrue();
	}

	@After
	public void tearDown() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.clear();
	}

	@Test
	public void newUninitializedBeanFactorLocator() {
		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator();

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isNull();
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isEmpty();
	}

	@Test
	public void newInitializedBeanFactoryLocator() {
		when(mockBeanFactory.getAliases(anyString())).thenReturn(new String[0]);

		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator(mockBeanFactory, "AssociatedBeanName");

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("AssociatedBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).hasSize(1);
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).containsAll(asSet("AssociatedBeanName"));

		verify(mockBeanFactory, times(1)).getAliases(eq("AssociatedBeanName"));
	}

	@Test
	public void newInitializedBeanFactoryLocatorWithNullBeanFactoryAndSpecifiedBeanName() {
		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator(null, "MyBeanName");

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isNull();
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("MyBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isEmpty();
	}

	@Test
	public void newInitializedBeanFactoryLocatorWithNonNullBeanFactoryAndUnspecifiedBeanNameThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("associatedBeanName must be specified when BeanFactory is not null");

		newBeanFactoryLocator(mockBeanFactory, "  ");
	}

	@Test
	public void resolveBeanFactoryReturnsResolvedBeanFactory() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("MyBeanKey", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(1);
		assertThat(GemfireBeanFactoryLocator.resolveBeanFactory("MyBeanKey")).isSameAs(mockBeanFactory);
	}

	@Test
	public void resolveBeanFactoryWithNoRegisteredBeanFactoriesAndAnyKeyReturnsNull() {
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).isEmpty();
		assertThat(GemfireBeanFactoryLocator.resolveBeanFactory("MyBeanKey")).isNull();
		assertThat(GemfireBeanFactoryLocator.resolveBeanFactory("AnotherBeanKey")).isNull();
		assertThat(GemfireBeanFactoryLocator.resolveBeanFactory("YetAnotherBeanKey")).isNull();
	}

	@Test
	public void resolveBeanFactoryWithRegisteredBeanFactoriesAndUnknownKeyThrowsIllegalArgumentException() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("MyBeanKey", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(1);

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory for key [UnknownKey] was not found");

		GemfireBeanFactoryLocator.resolveBeanFactory("UnknownKey");
	}

	@Test
	public void resolveSingleBeanFactoryWithNoRegisteredBeanFactoriesReturnsNull() {
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.isEmpty()).isTrue();
		assertThat(GemfireBeanFactoryLocator.resolveSingleBeanFactory()).isNull();
	}

	@Test
	public void resolveSingleBeanFactoryWhenSingleBeanFactoryIsRegisteredReturnsSingleBeanFactory() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("MyBeanKey", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(1);
		assertThat(GemfireBeanFactoryLocator.resolveSingleBeanFactory()).isSameAs(mockBeanFactory);
	}

	@Test
	public void resolveSingleBeanFactoryWhenMultipleIdenticalBeanFactoriesAreRegisteredReturnsSingleBeanFactory() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);
		assertThat(GemfireBeanFactoryLocator.resolveSingleBeanFactory()).isSameAs(mockBeanFactory);
	}

	@Test
	public void resolveSingeBeanFactoryWhenMultipleDifferentBeanFactoriesAreRegisteredThrowsIllegalStateException() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refTwo", mock(BeanFactory.class));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory key must be specified when more than one BeanFactory [refOne, refTwo]"
			+ " is registered");

		GemfireBeanFactoryLocator.resolveSingleBeanFactory();
	}

	@Test
	public void registerAliasesIsSuccessful() {
		Set<String> aliases = asSet("aliasOne", "aliasTwo", "aliasThree");

		GemfireBeanFactoryLocator.registerAliases(aliases, mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(3);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.keySet()).containsAll(aliases);

		Set<BeanFactory> beanFactories = new HashSet<>(GemfireBeanFactoryLocator.BEAN_FACTORIES.values());

		assertThat(beanFactories).hasSize(1);
		assertThat(beanFactories).containsAll(asSet(mockBeanFactory));
	}

	@Test
	public void registerAliasesWithEmptyAliasesAndNonNullBeanFactoryDoesNothing() {
		GemfireBeanFactoryLocator.registerAliases(Collections.emptySet(), mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.isEmpty()).isTrue();
	}

	@Test
	public void registerAliasesWithEmptyAliasesAndNullBeanFactoryDoesNothing() {
		GemfireBeanFactoryLocator.registerAliases(Collections.emptySet(), null);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.isEmpty()).isTrue();
	}

	@Test
	public void registerAliasesWithNonEmptyAliasesAndNullBeanFactoryThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory must not be null when aliases are specified");

		GemfireBeanFactoryLocator.registerAliases(asSet("aliasOne", "aliasTwo"), null);
	}

	@Test
	public void registerAliasesWithNullAliasesHandlesNullAndDoesNothing() {
		GemfireBeanFactoryLocator.registerAliases(null, null);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.isEmpty()).isTrue();
	}

	@Test
	public void registerAliasesWhenIdenticalBeanFactoryReferencesAlreadyExistIsSuccessful() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(1);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("aliasTwo")).isSameAs(mockBeanFactory);

		GemfireBeanFactoryLocator.registerAliases(asSet("aliasOne", "aliasTwo"), mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("aliasOne")).isSameAs(mockBeanFactory);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("aliasTwo")).isSameAs(mockBeanFactory);
	}

	@Test
	public void registerAliasesWhenNonIdenticalBeanFactoryReferencesAlreadyExistThrowsIllegalArgumentException() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(1);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("aliasTwo")).isSameAs(mockBeanFactory);

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory reference already exists for key [aliasTwo]");

		GemfireBeanFactoryLocator.registerAliases(asSet("aliasOne", "aliasTwo"), mock(BeanFactory.class));
	}

	@Test
	public void unregisterAliasesRemovesAll() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);

		GemfireBeanFactoryLocator.unregisterAliases(asSet("aliasOne", "aliasTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.isEmpty()).isTrue();
	}

	@Test
	public void unregisterAliasesRemovesPartial() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);

		GemfireBeanFactoryLocator.unregisterAliases(asSet("aliasTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(1);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.containsKey("aliasOne")).isTrue();
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.containsKey("aliasTwo")).isFalse();
	}

	@Test
	public void unregisterAliasesRemovesNone() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("aliasTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);

		GemfireBeanFactoryLocator.unregisterAliases(asSet("refOne", "refTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.containsKey("aliasOne")).isTrue();
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.containsKey("aliasTwo")).isTrue();
	}

	@Test
	public void afterPropertiesSetResolvesAndInitializesBeanNamesWithAliasesThenRegisterAliases() {
		when(mockBeanFactory.getAliases(eq("AssociatedBeanName"))).thenReturn(new String[] { "aliasOne", "aliasTwo" });

		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator(mockBeanFactory, "AssociatedBeanName");

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("AssociatedBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).hasSize(3);
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases())
			.containsAll(asSet("AssociatedBeanName", "aliasOne", "aliasTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(3);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.keySet())
			.containsAll(asSet("AssociatedBeanName", "aliasOne", "aliasTwo"));
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("AssociatedBeanName")).isSameAs(mockBeanFactory);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("aliasOne")).isSameAs(mockBeanFactory);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.get("aliasTwo")).isSameAs(mockBeanFactory);

		verify(mockBeanFactory, times(1)).getAliases(eq("AssociatedBeanName"));
	}

	@Test
	public void afterPropertiesSetUnableToResolveInitializeAndRegisterAliasesWithNullBeanFactory() {
		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator(null, "AssociatedBeanName");

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isNull();
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("AssociatedBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isEmpty();
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).isEmpty();
	}

	@Test
	public void destroyUnregistersOwningAliases() {
		BeanFactory mockBeanFactoryTwo = mock(BeanFactory.class, "MockBeanFactoryTwo");

		when(mockBeanFactory.getAliases(eq("AssociatedBeanName"))).thenReturn(new String[] { "aliasOne", "aliasTwo" });

		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refOne", mockBeanFactoryTwo);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refTwo", mockBeanFactoryTwo);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(2);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.keySet()).containsAll(asSet("refOne", "refTwo"));

		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator(mockBeanFactory, "AssociatedBeanName");

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("AssociatedBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).hasSize(3);
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).containsAll(
			asSet("AssociatedBeanName", "aliasOne", "aliasTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(5);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.keySet())
			.containsAll(asSet("refOne", "refTwo", "AssociatedBeanName", "aliasOne", "aliasTwo"));

		beanFactoryLocator.destroy();

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(2);
		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.keySet()).containsAll(asSet("refOne", "refTwo"));

		verify(mockBeanFactory, times(1)).getAliases(eq("AssociatedBeanName"));
		verifyZeroInteractions(mockBeanFactoryTwo);
	}

	@Test
	public void useBeanFactoryWhenNoBeanFactoriesAreRegisteredThrowsIllegalStateException() {
		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(UNINITIALIZED_BEAN_FACTORY_REFERENCE_MESSAGE);

		newBeanFactoryLocator().useBeanFactory();
	}

	@Test
	public void useBeanFactoryWhenSingleBeanFactoryIsRegisteredReturnsSingleBeanFactory() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refTwo", mockBeanFactory);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);

		assertThat(newBeanFactoryLocator().useBeanFactory()).isSameAs(mockBeanFactory);
	}

	@Test
	public void useBeanFactoryWhenMultipleBeanFactoriesAreRegisteredThrowsIllegalStateException() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refTwo", mock(BeanFactory.class, "MockBeanFactoryTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(2);

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory key must be specified when more than one BeanFactory [refOne, refTwo]"
			+ " is registered");

		newBeanFactoryLocator().useBeanFactory();
	}

	@Test
	public void useBeanFactoryWhenMultipleBeanFactoriesAreRegisteredWithConfiguredKeyReturnsBeanFactory() {
		BeanFactory mockBeanFactoryTwo = mock(BeanFactory.class, "MockBeanFactoryTwo");

		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("refTwo", mockBeanFactoryTwo);

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(2);
		assertThat(newBeanFactoryLocator(null, "refOne").useBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(newBeanFactoryLocator().withBeanName("refTwo").useBeanFactory()).isSameAs(mockBeanFactoryTwo);
	}

	@Test
	public void useBeanFactoryWithKeyReturnsSpecificBeanFactory() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("keyOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("keyTwo", mock(BeanFactory.class, "MockBeanFactoryTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES).hasSize(2);
		assertThat(newBeanFactoryLocator().useBeanFactory("keyOne")).isSameAs(mockBeanFactory);
	}

	@Test
	public void useBeanFactoryWithUnknownKeyThrowsIllegalArgumentException() {
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("keyOne", mockBeanFactory);
		GemfireBeanFactoryLocator.BEAN_FACTORIES.put("keyTwo", mock(BeanFactory.class, "MockBeanFactoryTwo"));

		assertThat(GemfireBeanFactoryLocator.BEAN_FACTORIES.size()).isEqualTo(2);

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory for key [UnknownKey] was not found");

		newBeanFactoryLocator().useBeanFactory("UnknownKey");
	}

	@Test
	public void setAndGetBeanFactory() {
		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator();

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getBeanFactory()).isNull();

		beanFactoryLocator.setBeanFactory(mockBeanFactory);

		assertThat(beanFactoryLocator.getBeanFactory()).isSameAs(mockBeanFactory);

		beanFactoryLocator.setBeanFactory(null);

		assertThat(beanFactoryLocator.getBeanFactory()).isNull();

		verifyZeroInteractions(mockBeanFactory);
	}

	@Test
	public void setAndGetAssociatedBeanName() {
		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator(null, "AssociatedBeanName");

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("AssociatedBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isEmpty();

		beanFactoryLocator.setBeanName("TestBeanName");

		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("TestBeanName");
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isEmpty();

		beanFactoryLocator.setBeanName(null);

		assertThat(beanFactoryLocator.getAssociatedBeanName()).isNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanNameWithAliases()).isEmpty();
	}

	@Test
	public void withBeanNameIsSuccessful() {
		GemfireBeanFactoryLocator beanFactoryLocator = newBeanFactoryLocator();

		assertThat(beanFactoryLocator).isNotNull();
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isNull();
		assertThat(beanFactoryLocator.withBeanName("MyBeanName")).isSameAs(beanFactoryLocator);
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isEqualTo("MyBeanName");
		assertThat(beanFactoryLocator.withBeanName(null)).isSameAs(beanFactoryLocator);
		assertThat(beanFactoryLocator.getAssociatedBeanName()).isNull();
	}
}
