#region Copyright (C) 2005-2009 Team MediaPortal

// Copyright (C) 2005-2009 Team MediaPortal
// http://www.team-mediaportal.com
// 
// This Program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
// 
// This Program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with GNU Make; see the file COPYING.  If not, write to
// the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
// http://www.gnu.org/copyleft/gpl.html

#endregion

using System;
using System.Runtime.Serialization;

namespace Commands
{
  /// <summary>
  /// The exception that is thrown when a structural error in a command is discovered.
  /// </summary>
  [Serializable]
  public class CommandStructureException : Exception
  {
    /// <summary>
    /// Initializes a new instance of the <see cref="CommandStructureException"/> class.
    /// </summary>
    public CommandStructureException()
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandStructureException"/> class.
    /// </summary>
    /// <param name="message">The message.</param>
    public CommandStructureException(string message) : base(message)
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandStructureException"/> class.
    /// </summary>
    /// <param name="message">The message.</param>
    /// <param name="innerException">The inner exception.</param>
    public CommandStructureException(string message, Exception innerException) : base(message, innerException)
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandStructureException"/> class.
    /// </summary>
    /// <param name="info">The object that holds the serialized object data.</param>
    /// <param name="context">The contextual information about the source or destination.</param>
    protected CommandStructureException(SerializationInfo info, StreamingContext context) : base(info, context)
    {
    }
  }
}