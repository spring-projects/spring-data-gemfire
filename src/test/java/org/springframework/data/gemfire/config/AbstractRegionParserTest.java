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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The AbstractRegionParserTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractRegionParser class.
 * <p/>
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
