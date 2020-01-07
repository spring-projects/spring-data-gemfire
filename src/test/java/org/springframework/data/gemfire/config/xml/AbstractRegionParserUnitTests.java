/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.matches;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.beans.factory.xml.XmlReaderContext;

/**
 * Unit tests for {@link AbstractRegionParser}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.xml.AbstractRegionParser
 * @since 1.3.3
 */
// TODO add more tests
public class AbstractRegionParserUnitTests {

	private AbstractRegionParser regionParser = new TestRegionParser();

	protected void assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate(String localName) {

		Element mockElement = mock(Element.class);

		when(mockElement.getLocalName()).thenReturn(localName);

		assertThat(regionParser.isRegionTemplate(mockElement)).isEqualTo(nullSafeEndsWith(localName, "-template"));

		verify(mockElement, times(1)).getLocalName();
	}

	protected void assertIsSubRegionWhenElementLocalNameEndsWithRegion(String localName) {

		Element mockElement = mock(Element.class);

		Node mockNode = mock(Node.class);

		when(mockElement.getParentNode()).thenReturn(mockNode);
		when(mockNode.getLocalName()).thenReturn(localName);

		assertThat(regionParser.isSubRegion(mockElement)).isEqualTo(nullSafeEndsWith(localName, "region"));

		verify(mockElement, times(1)).getParentNode();
		verify(mockNode, times(1)).getLocalName();
	}

	protected boolean nullSafeEndsWith(String localName, String suffix) {
		return localName != null && localName.endsWith(suffix);
	}

	@Test
	public void getBeanClassIsEqualToTestRegionFactoryBean() {

		AbstractRegionParser regionParserSpy = spy(AbstractRegionParser.class);

		Element mockElement = mock(Element.class);

		doReturn(AbstractRegionParserUnitTests.class).when(regionParserSpy).getRegionFactoryClass();

		assertThat(regionParserSpy.getBeanClass(mockElement)).isEqualTo(AbstractRegionParserUnitTests.class);

		verify(regionParserSpy, times(1)).getBeanClass(eq(mockElement));
		verify(regionParserSpy, times(1)).getRegionFactoryClass();
	}

	@Test
	public void getParentNameWhenTemplateIsSet() {

		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq("template"))).thenReturn("test");

		assertThat(regionParser.getParentName(mockElement)).isEqualTo("test");

		verify(mockElement, times(1)).getAttribute(eq("template"));
	}

	@Test
	public void getParentNameWhenTemplateIsUnset() {

		Element mockElement = mock(Element.class);

		when(mockElement.getAttribute(eq("template"))).thenReturn(null);

		assertThat(regionParser.getParentName(mockElement)).isNull();

		verify(mockElement, times(1)).getAttribute(eq("template"));
	}

	@Test
	public void isRegionTemplateWithRegionTemplateElementsIsTrue() {

		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("client-region-template");
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("local-region-template");
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("partitioned-region-template");
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("replicated-region-template");
	}

	@Test
	public void isRegionTemplateWithRegionElementsIsFalse() {

		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("client-region");
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("local-region");
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("partitioned-region");
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("replicated-region");
	}

	@Test
	public void isRegionTemplateWhenElementLocalNameDoesNotEndWithTemplateIsFalse() {
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate("disk-store");
	}

	@Test
	public void isRegionTemplateWhenElementLocalNameIsNullIsFalse() {
		assertIsRegionTemplateWhenElementLocalNameEndsWithTemplate(null);
	}

	@Test
	public void isSubRegionWithRegionElementIsTrue() {

		assertIsSubRegionWhenElementLocalNameEndsWithRegion("client-region");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("local-region");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("partitioned-region");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("replicated-region");
	}

	@Test
	public void isSubRegionWithRegionTemplateElementIsFalse() {

		assertIsSubRegionWhenElementLocalNameEndsWithRegion("client-region-template");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("local-region-template");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("partitioned-region-template");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("replicated-region-template");
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("region-template");
	}

	@Test
	public void isSubRegionWhenElementLocalNameDoesNotEndWithRegionIsFalse() {
		assertIsSubRegionWhenElementLocalNameEndsWithRegion("disk-store");
	}

	@Test
	public void isSubRegionWhenElementLocalNameIsNullIsFalse() {
		assertIsSubRegionWhenElementLocalNameEndsWithRegion(null);
	}

	@Test
	public void doParseWithAbstractRegionTemplate() {

		AbstractRegionParser regionParserSpy = spy(AbstractRegionParser.class);

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition();

		Element mockElement = mock(Element.class);

		Node mockNode = mock(Node.class);

		when(mockElement.getLocalName()).thenReturn("partitioned-region-template");
		when(mockElement.getParentNode()).thenReturn(mockNode);
		when(mockNode.getLocalName()).thenReturn("cache");

		regionParserSpy.doParse(mockElement, null, builder);

		assertThat(builder.getRawBeanDefinition().isAbstract()).isTrue();

		verify(regionParserSpy, times(1)).doParse(eq(mockElement), isNull(), eq(builder));
		verify(regionParserSpy, times(1)).doParseRegion(eq(mockElement), isNull(),
			eq(builder), eq(false));
	}

	@Test
	public void doParseWithNonAbstractSubRegion() {

		AbstractRegionParser regionParserSpy = spy(AbstractRegionParser.class);

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition();

		Element mockElement = mock(Element.class);
		Node mockNode = mock(Node.class);

		when(mockElement.getLocalName()).thenReturn("replicated-region");
		when(mockElement.getParentNode()).thenReturn(mockNode);
		when(mockNode.getLocalName()).thenReturn("replicated-region");

		regionParserSpy.doParse(mockElement, null, builder);

		assertThat(builder.getRawBeanDefinition().isAbstract()).isFalse();

		verify(regionParserSpy, times(1)).doParse(eq(mockElement), isNull(), eq(builder));
		verify(regionParserSpy, times(1)).doParseRegion(eq(mockElement), isNull(),
			eq(builder), eq(true));
	}

	@Test
	public void validateDataPolicyShortcutAttributesMutualExclusion() {

		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(false);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(false);

		this.regionParser.validateDataPolicyShortcutAttributesMutualExclusion(mockElement, null);

		verify(mockElement).hasAttribute(eq("data-policy"));
		verify(mockElement, never()).hasAttribute(eq("shortcut"));
	}

	@Test
	public void validateDataPolicyShortcutAttributesMutualExclusionWithDataPolicy() {

		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(true);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(false);

		this.regionParser.validateDataPolicyShortcutAttributesMutualExclusion(mockElement, null);

		verify(mockElement).hasAttribute(eq("data-policy"));
		verify(mockElement).hasAttribute(eq("shortcut"));
	}

	@Test
	public void validateDataPolicyShortcutAttributesMutualExclusionWithShortcut() {

		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(false);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(true);

		this.regionParser.validateDataPolicyShortcutAttributesMutualExclusion(mockElement, null);

		verify(mockElement).hasAttribute(eq("data-policy"));
		verify(mockElement, never()).hasAttribute(eq("shortcut"));
	}

	@Test
	public void validateDataPolicyShortcutAttributesMutualExclusionWithDataPolicyAndShortcut() {

		Element mockElement = mock(Element.class);

		XmlReaderContext mockReaderContext = mock(XmlReaderContext.class);

		ParserContext mockParserContext = new ParserContext(mockReaderContext, null);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(true);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(true);
		when(mockElement.getTagName()).thenReturn("local-region");

		this.regionParser.validateDataPolicyShortcutAttributesMutualExclusion(mockElement, mockParserContext);

		verify(mockReaderContext).error(
			eq("Only one of [data-policy, shortcut] may be specified with element [local-region]"),
				eq(mockElement));
	}

	protected static class TestRegionParser extends AbstractRegionParser {

		@Override
		protected Class<?> getRegionFactoryClass() {
			return getClass();
		}

		@Override
		protected void doParseRegion(Element element, ParserContext parserContext,
				BeanDefinitionBuilder builder, boolean subRegion) {

			throw new UnsupportedOperationException("Not Implemented");
		}
	}
}
