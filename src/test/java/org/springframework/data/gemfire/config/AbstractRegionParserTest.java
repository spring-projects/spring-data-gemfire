/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.matches;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.beans.factory.xml.XmlReaderContext;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The AbstractRegionParserTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractRegionParser class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.AbstractRegionParser
 * @since 1.3.3
 */
public class AbstractRegionParserTest {

	private AbstractRegionParser regionParser = new TestRegionParser();

	@Test
	public void testIsSubRegionWhen() {
		Element mockElement = mock(Element.class);
		Node mockNode = mock(Node.class);

		when(mockElement.getParentNode()).thenReturn(mockNode);
		when(mockNode.getLocalName()).thenReturn("partitioned-region");

		assertTrue(regionParser.isSubRegion(mockElement));
	}

	@Test
	public void testIsSubRegionWhenLocalNameIsNull() {
		Element mockElement = mock(Element.class);
		Node mockNode = mock(Node.class);

		when(mockElement.getParentNode()).thenReturn(mockNode);
		when(mockNode.getLocalName()).thenReturn(null);

		assertFalse(regionParser.isSubRegion(mockElement));
	}

	@Test
	public void testIsSubRegionWhenLocalNameDoesNotEndWithRegion() {
		Element mockElement = mock(Element.class);
		Node mockNode = mock(Node.class);

		when(mockElement.getParentNode()).thenReturn(mockNode);
		when(mockNode.getLocalName()).thenReturn("disk-store");

		assertFalse(regionParser.isSubRegion(mockElement));
	}

	@Test
	public void testValidateDataPolicyShortcutAttributesMutualExclusion() {
		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(false);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(false);

		new TestRegionParser().validateDataPolicyShortcutAttributesMutualExclusion(mockElement, null);

		verify(mockElement).hasAttribute(eq("data-policy"));
		verify(mockElement, never()).hasAttribute(eq("shortcut"));
	}

	@Test
	public void testValidateDataPolicyShortcutAttributesMutualExclusionWithDataPolicy() {
		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(true);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(false);

		new TestRegionParser().validateDataPolicyShortcutAttributesMutualExclusion(mockElement, null);

		verify(mockElement).hasAttribute(eq("data-policy"));
		verify(mockElement).hasAttribute(eq("shortcut"));
	}

	@Test
	public void testValidateDataPolicyShortcutAttributesMutualExclusionWithShortcut() {
		Element mockElement = mock(Element.class);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(false);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(true);

		new TestRegionParser().validateDataPolicyShortcutAttributesMutualExclusion(mockElement, null);

		verify(mockElement).hasAttribute(eq("data-policy"));
		verify(mockElement, never()).hasAttribute(eq("shortcut"));
	}

	@Test
	public void testValidateDataPolicyShortcutAttributesMutualExclusionWithDataPolicyAndShortcut() {
		Element mockElement = mock(Element.class);
		XmlReaderContext mockReaderContext = mock(XmlReaderContext.class);

		ParserContext mockParserContext = new ParserContext(mockReaderContext, null);

		when(mockElement.hasAttribute(matches("data-policy"))).thenReturn(true);
		when(mockElement.hasAttribute(matches("shortcut"))).thenReturn(true);
		when(mockElement.getTagName()).thenReturn("local-region");

		new TestRegionParser().validateDataPolicyShortcutAttributesMutualExclusion(mockElement, mockParserContext);

		verify(mockReaderContext).error(
			eq("Only one of [data-policy, shortcut] may be specified with element 'local-region'."),
				eq(mockElement));
	}

	protected static class TestRegionParser extends AbstractRegionParser {

		@Override
		protected Class<?> getRegionFactoryClass() {
			return getClass();
		}

		@Override
		protected void doParseRegion(final Element element, final ParserContext parserContext,
				final BeanDefinitionBuilder builder, final boolean subRegion) {
			throw new UnsupportedOperationException("Not Implemented!");
		}
	}

}
